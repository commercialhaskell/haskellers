{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Data.IORef
import Control.Monad
import Control.Concurrent
import Database.Persist.Sql
import Data.Maybe
import qualified Data.Set as Set
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import System.Environment (lookupEnv)
import System.Timeout
import Network.Mail.Mime.SES
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import Control.Exception (throwIO)
import Data.Yaml (decodeFileEither)

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

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

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

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager tlsManagerSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/db/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)
    --runNoLoggingT $ Database.Persist.runPool dbconf (runMigration migrateAll) p

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
        , homepageProfiles = hprofs
        , publicProfiles = pprofs
        , sesCreds = \email -> SES
                { sesFrom = "webmaster@haskellers.com"
                , sesTo = [encodeUtf8 email]
                , sesAccessKey = S8.pack access
                , sesSecretKey = S8.pack secret
                , sesRegion = usEast1
                }
        , appGoogleEmailCreds = googleEmailCreds
        , appFacebookCreds = facebookCreds
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
