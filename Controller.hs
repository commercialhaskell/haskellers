{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withHaskellers
    , withDevelApp
    ) where

import Haskellers
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql
import Data.IORef
#if PRODUCTION
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
#endif
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)

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
withHaskellers :: (Application -> IO a) -> IO a
withHaskellers f = Settings.withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration migrateAll
    hprofs <- newIORef ([], 0)
    pprofs <- newIORef []
#if PRODUCTION
    _ <- forkIO $ forever $ fillProfs p hprofs pprofs
                         >> threadDelay (1000 * 1000 * 60 * 5)
#else
    fillProfs p hprofs pprofs
#endif
    let h = Haskellers s p hprofs pprofs
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withHaskellers :: (Application -> IO ()) -> IO ())

getHomepageProfs :: ConnectionPool -> IO [Profile]
getHomepageProfs pool = flip runConnectionPool pool $ do
    users <-
        selectList [ UserVerifiedEmailEq True
                   , UserVisibleEq True
                   , UserRealEq True
                   , UserBlockedEq False
                   -- FIXME , UserRealPicEq True
                   ] [] 0 0
    fmap catMaybes $ mapM userToProfile users

getPublicProfs :: ConnectionPool -> IO [Profile]
getPublicProfs pool = flip runConnectionPool pool $ do
    users <-
        selectList [ UserVerifiedEmailEq True
                   , UserVisibleEq True
                   , UserBlockedEq False
                   ]
                   [ UserRealDesc
                   , UserHaskellSinceAsc
                   , UserFullNameAsc
                   ] 0 0
    fmap catMaybes $ mapM userToProfile users

fillProfs :: ConnectionPool -> IORef ([Profile], Int) -> IORef [Profile] -> IO ()
fillProfs pool hprofs pprofs = do
    hprofs' <- getHomepageProfs pool
    pprofs' <- getPublicProfs pool
    writeIORef hprofs (hprofs', length hprofs')
    writeIORef pprofs pprofs'

userToProfile :: (Functor m, PersistBackend m) => (UserId, User) -> m (Maybe Profile)
userToProfile (uid, u) =
    case userEmail u of
        Nothing -> return Nothing
        Just e -> do
            mun <- fmap (fmap snd) $ getBy $ UniqueUsernameUser uid
            return $ Just Profile
                { profileUserId = uid
                , profileName = userFullName u
                , profileEmail = e
                , profileUser = u
                , profileSkills = Set.fromList [] -- FIXME
                , profileUsername = mun
                }
