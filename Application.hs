{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Foundation hiding (approot)
import Settings
import Yesod.Static
import Yesod.Auth
import Database.Persist.GenericSql
import Data.IORef
import Data.Text (Text)
#if PRODUCTION
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
#endif
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
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
-- of the call to mkYesodData which occurs in Haskellers.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Haskellers" resourcesHaskellers

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: Text -> IO Application
getApplication approot = do
    p <- Settings.createConnectionPool
    manager <- newManager def
    flip runConnectionPool p $ runMigration migrateAll
    hprofs <- newIORef ([], 0)
    pprofs <- newIORef []
#if PRODUCTION
    _ <- forkIO $ forever $ fillProfs p hprofs pprofs
                         >> threadDelay (1000 * 1000 * 60 * 5)
#else
    fillProfs p hprofs pprofs
#endif
    s' <- s
    let h = Haskellers s' p hprofs pprofs approot manager
    toWaiApp h
  where
    s = static Settings.staticdir

getApplicationDev :: IO (Int, Application)
getApplicationDev = ((,) 3000) `fmap` getApplication "http://localhost:3000"

getHomepageProfs :: ConnectionPool -> IO [Profile]
getHomepageProfs pool = flip runConnectionPool pool $ do
    users <-
        selectList [ UserVerifiedEmail ==. True
                   , UserVisible ==. True
                   , UserReal ==. True
                   , UserBlocked ==. False
                   -- FIXME , UserRealPicEq True
                   ] []
    fmap catMaybes $ mapM userToProfile users

getPublicProfs :: ConnectionPool -> IO [Profile]
getPublicProfs pool = flip runConnectionPool pool $ do
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

userToProfile :: (Functor (b m), PersistUnique b m, b ~ SqlPersist) => Entity User -> b m (Maybe Profile)
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
                }
