{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where


import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Data.IORef
import Data.Pool
import qualified Data.Set as Set
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Mail.Mime.SES
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Root
import Handler.Profile
import Handler.User
import Handler.Admin
import Handler.Email
import Handler.Skills
import Handler.Package
import Handler.Faq
import Handler.News
import Handler.Job
import Handler.Team
import Handler.Topic
import Handler.Bling
import Handler.Poll

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)


    hprofs <- newIORef ([], 0)
    pprofs <- newIORef []
    if production
        then do
            _ <- forkIO $ forever $ do
                _ <- forkIO $ do
                    _ <- timeout (1000 * 1000 * 60 * 2) $ fillProfs p hprofs pprofs
                    return ()
                threadDelay (1000 * 1000 * 60 * 10)
            return ()
        else fillProfs p hprofs pprofs

    appHomepageProfiles = hprofs
    appPublicProfiles = pprofs

    appGoogleEmailCreds = googleEmailCreds
    appFacebookCreds = facebookCreds

    appSesCreds = \email -> SES
            { sesFrom = "webmaster@haskellers.com"
            , sesTo = [encodeUtf8 email]
            , sesAccessKey = S8.pack access
            , sesSecretKey = S8.pack secret
            , sesRegion = usEast1
            }

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createMySQLPool
        (myConnInfo $ appDatabaseConf appSettings)
        (myPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiApp foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation_REMOVE :: AppConfig DefaultEnv Extra -> IO App
makeFoundation_REMOVE conf = do
    manager <- newManager tlsManagerSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/db/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)
    runNoLoggingT $ Database.Persist.runPool dbconf (runMigration migrateAll) p

    hprofs <- newIORef ([], 0)
    pprofs <- newIORef []
    if production
        then do
            _ <- forkIO $ forever $ do
                _ <- forkIO $ do
                    _ <- timeout (1000 * 1000 * 60 * 2) $ fillProfs p hprofs pprofs
                    return ()
                threadDelay (1000 * 1000 * 60 * 10)
            return ()
        else fillProfs p hprofs pprofs

    maccess <- lookupEnv "AWS_ACCESS_KEY"
    msecret <- lookupEnv "AWS_SECRET_KEY"
    (access, secret) <-
        case (,) <$> maccess <*> msecret of
            Just pair -> return pair
            Nothing -> do
                m <- decodeFileEither "config/db/aws" >>= either throwIO return
                case (,) <$> Map.lookup "access" m <*> Map.lookup ("secret" :: Text) m of
                    Just pair -> return pair
                    Nothing -> error $ "Invalid config/db/aws: " ++ show m

    googleEmailCreds <- do
        m <- decodeFileEither "config/db/google-email.yaml" >>= either throwIO return
        case (,) <$> Map.lookup "client-id" m <*> Map.lookup ("client-secret" :: Text) m of
            Just pair -> return pair
            Nothing -> error $ "Invalid config/db/google-email.yaml: " ++ show m

    facebookCreds <- do
        m <- decodeFileEither "config/db/facebook.yaml" >>= either throwIO return
        case (,,)
          <$> Map.lookup ("name" :: Text) m
          <*> Map.lookup "id" m
          <*> Map.lookup "secret" m of
            Just x -> return x
            Nothing -> error $ "Invalid config/db/facebook.yaml: " ++ show m

    return $ App
        { settings = conf
        , getStatic = s
        , connPool = p
        , httpManager = manager
        , persistConfig = dbconf

        }

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

getHomepageProfs :: ConnectionPool -> IO [Profile]
getHomepageProfs pool = runNoLoggingT $ runResourceT $ flip runSqlPool pool $ do
    users <-
        selectList [ UserVerifiedEmail ==. True
                   , UserVisible ==. True
                   , UserReal ==. True
                   , UserBlocked ==. False
                   -- FIXME , UserRealPicEq True
                   ] []
    fmap catMaybes $ mapM userToProfile users

getPublicProfs :: ConnectionPool -> IO [Profile]
getPublicProfs pool = runNoLoggingT $ runResourceT $ flip runSqlPool pool $ do
    users <-
        selectList [ UserVerifiedEmail ==. True
                   , UserVisible ==. True
                   , UserBlocked ==. False
                   ]
                   [ Desc UserReal
                   , Asc UserHaskellSince
                   , Asc UserFullName
                   ]
    fmap catMaybes $ mapM userToProfile users

fillProfs :: ConnectionPool -> IORef ([Profile], Int) -> IORef [Profile] -> IO ()
fillProfs pool hprofs pprofs = do
    hprofs' <- getHomepageProfs pool
    pprofs' <- getPublicProfs pool
    writeIORef hprofs (hprofs', length hprofs')
    writeIORef pprofs pprofs'

userToProfile :: (MonadLogger m, MonadResource m) => Entity User -> SqlPersistT m (Maybe Profile)
userToProfile (Entity uid u) =
    case userEmail u of
        Nothing -> return Nothing
        Just e -> do
            mun <- fmap (fmap entityVal) $ getBy $ UniqueUsernameUser uid
            return $ Just Profile
                { profileUserId = uid
                , profileName = userFullName u
                , profileEmail = e
                , profileUser = u
                , profileSkills = Set.fromList [] -- FIXME
                , profileUsername = mun
                , profileLocation = Location <$> userLongitude u <*> userLatitude u
                }
