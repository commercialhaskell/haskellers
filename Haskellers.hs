{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Haskellers
    ( Haskellers (..)
    , HaskellersRoute (..)
    , resourcesHaskellers
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , maybeAuth'
    , requireAuth'
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    , login
    , Profile (..)
    , userR
    , debugRunDBInner
    , getDebugR
    , prettyTime
    , prettyDay
    , addTeamNews
    ) where

#define debugRunDB debugRunDBInner __FILE__ __LINE__

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Facebook
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile)
import Model
import StaticFiles (logo_png, jquery_ui_css, google_png, yahoo_png,
                    openid_icon_small_gif, facebook_png)
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Data.IORef (IORef)
import qualified Data.Set as Set

import Control.Concurrent.STM
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Invert (finally)

import Data.Time
import System.Locale

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Haskellers = Haskellers
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    , homepageProfiles :: IORef ([Profile], Int)
    , publicProfiles :: IORef [Profile]
    }

data Profile = Profile
    { profileUserId :: UserId
    , profileName :: String
    , profileEmail :: String
    , profileUser :: User
    , profileSkills :: Set.Set SkillId
    , profileUsername :: Maybe Username
    }
  deriving Show

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Haskellers Haskellers
type Widget = GWidget Haskellers Haskellers

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype HaskellersRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Haskellers = HaskellersRoute
-- * Creates the value resourcesHaskellers which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Haskellers. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the HaskellersRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Haskellers" [$parseRoutes|
/static StaticR Static getStatic
/auth   AuthR   Auth   getAuth

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ RootR GET
/users UsersR GET
/locations LocationsR GET
/page/faq FaqR GET

/profile ProfileR GET POST
/profile/delete DeleteAccountR POST
/profile/skills SkillsR POST
/profile/ident/#IdentId/delete DeleteIdentR POST
/profile/request-real RequestRealR POST
/profile/request-realpic RequestRealPicR POST
/profile/request-unblock RequestUnblockR POST
/profile/request-skill RequestSkillR POST
/profile/username SetUsernameR POST
/profile/clear-username ClearUsernameR POST
/profile/screen-names ScreenNamesR POST
/profile/screen-names/#ScreenNameId/delete DeleteScreenNameR POST

/skills AllSkillsR GET POST
/skills/#SkillId SkillR GET

/packages PackagesR POST
/package/#PackageId/delete DeletePackageR POST

/user/#String UserR GET
/user/#UserId/admin AdminR POST
/user/#UserId/unadmin UnadminR POST

/user/#UserId/real RealR POST
/user/#UserId/unreal UnrealR POST

/user/#UserId/realpic RealPicR POST
/user/#UserId/unrealpic UnrealPicR POST

/user/#UserId/block BlockR POST
/user/#UserId/unblock UnblockR POST

/user/#UserId/flag FlagR GET POST

/user ByIdentR GET

/profile/reset-email ResetEmailR POST
/profile/send-verify SendVerifyR POST
/profile/verify/#String VerifyEmailR GET

/admin AdminUsersR GET
/admin/messages MessagesR GET
/admin/messages/#MessageId/close CloseMessageR POST

/news NewsR GET POST
/news/#NewsId NewsItemR GET

/debug DebugR GET

/jobs JobsR GET POST
/jobs/#JobId JobR GET

/feed/news NewsFeedR GET
/feed/jobs JobsFeedR GET
/feed/team/#TeamId TeamFeedR GET
/feed/user/#UserId UserFeedR GET
/feed/team-item/#TeamNewsId TeamNewsR GET

/teams TeamsR GET POST
/teams/#TeamId TeamR GET POST
/teams/#TeamId/leave LeaveTeamR POST
/teams/#TeamId/watch WatchTeamR POST
/teams/#TeamId/join JoinTeamR POST
/teams/#TeamId/approve/#UserId ApproveTeamR POST
/teams/#TeamId/admin/#UserId TeamAdminR POST
/teams/#TeamId/unadmin/#UserId TeamUnadminR POST
/teams/#TeamId/packages TeamPackagesR POST
/teams/#TeamId/packages/#TeamPackageId/delete DeleteTeamPackageR POST
|]

maybeAuth' :: GHandler s Haskellers (Maybe ((UserId, User), Maybe Username))
maybeAuth' = do
    x <- maybeAuth
    case x of
        Nothing -> return Nothing
        Just (uid, u) -> do
            y <- debugRunDB $ getBy $ UniqueUsernameUser uid
            return $ Just ((uid, u), fmap snd y)

requireAuth' :: GHandler s Haskellers ((UserId, User), Maybe Username)
requireAuth' = do
    (uid, u) <- requireAuth
    y <- debugRunDB $ getBy $ UniqueUsernameUser uid
    return ((uid, u), fmap snd y)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Haskellers where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        ma <- maybeAuth'
        y <- getYesod
        (title', parents) <- breadcrumbs
        current <- getCurrentRoute
        tm <- getRouteToMaster
        let title = if fmap tm current == Just RootR
                        then "Haskellers"
                        else title'
        let isCurrent :: HaskellersRoute -> Bool
            isCurrent RootR = fmap tm current == Just RootR
            isCurrent x = Just x == fmap tm current || x `elem` map fst parents
        let navbarSection section = $(hamletFile "navbar-section")
        pc <- widgetToPageContent $ do
            case ma of
                Nothing -> return ()
                Just ((uid, _), _) -> addHamletHead [$hamlet|%link!href=@UserFeedR.uid@!type="application/atom+xml"!rel="alternate"!title="Your Haskellers Updates"
|]
            widget
            addCassius $(Settings.cassiusFile "default-layout")
            addCassius $(Settings.cassiusFile "login")
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            addJulius $(Settings.juliusFile "analytics")
            addJulius $(Settings.juliusFile "default-layout")
        let login' = $(hamletFile "login")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static Haskellers ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        liftIO $ L.writeFile (statictmp ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

navbar :: [(String, [(String, HaskellersRoute)])]
navbar =
    [ ("General",
        [ ("Homepage", RootR)
        , ("FAQ", FaqR)
        , ("News Archive", NewsR)
        ]
      )
    , ("Find a Haskeller",
        [ ("Browse Users", UsersR)
        , ("Browse Skills", AllSkillsR)
        ]
      )
    , ("Find a Job",
        [ ("Job Listings", JobsR)
        ]
      )
    , ("Special Interest Groups",
        [ ("All Groups", TeamsR)
        ]
      )
    ]

userbar :: ((UserId, User), Maybe Username)
        -> [(String, [(String, HaskellersRoute)])]
userbar ((uid, u), a) = (:) ("Your Profile",
    [ ("Edit Profile", ProfileR)
    , ("View Profile", userR ((uid, u), a))
    , ("Logout", AuthR LogoutR)
    ])
    $ if userAdmin u
        then [("Administration",
                [ ("Messages", MessagesR)
                , ("All Users", AdminUsersR)
                ])]
        else []

loginbar :: (String, [(String, HaskellersRoute)])
loginbar = ("Account", [("Login", AuthR LoginR)])

instance YesodBreadcrumbs Haskellers where
    breadcrumb RootR = return ("Homepage", Nothing)
    breadcrumb FaqR = return ("Frequently Asked Questions", Just RootR)
    breadcrumb NewsR = return ("News Archive", Just RootR)
    breadcrumb (NewsItemR nid) = do
        n <- runDB $ get404 nid
        return (newsTitle n, Just NewsR)
    breadcrumb UsersR = return ("Browse Users", Just RootR)
    breadcrumb AllSkillsR = return ("Browse Skills", Just RootR)
    breadcrumb (SkillR sid) = do
        s <- runDB $ get404 sid
        return (skillName s, Just AllSkillsR)
    breadcrumb (FlagR uid) = return ("Report a User", Just $ UserR $ showIntegral uid)
    breadcrumb (UserR str) = do
        u <- runDB $
            case readIntegral str of
                Just uid -> get404 uid
                Nothing -> do
                    x <- getBy $ UniqueUsername str
                    case x of
                        Nothing -> lift notFound
                        Just (_, un) -> get404 $ usernameUser un
        return (userFullName u, Nothing)
    breadcrumb ProfileR = return ("Edit Your Profile", Just RootR)
    breadcrumb VerifyEmailR{} = return ("Verify Your Email Address", Nothing)
    breadcrumb AdminUsersR = return ("User List- Admin", Nothing)
    breadcrumb MessagesR = return ("Messages- Admin", Nothing)
    breadcrumb (AuthR LoginR) = return ("Log in to Haskellers", Just RootR)
    breadcrumb DebugR = return ("Database pool debug info", Just RootR)

    breadcrumb JobsR = return ("Job Listings", Just RootR)
    breadcrumb (JobR jid) = do
        j <- runDB $ get404 jid
        return ("Job Listing: " ++ jobTitle j, Just JobsR)
    breadcrumb TeamsR = return ("Special Interest Groups", Just RootR)
    breadcrumb (TeamR tid) = do
        t <- runDB $ get404 tid
        return (teamName t, Just TeamsR)
    breadcrumb (TeamPackagesR tid) = return ("Add Package", Just $ TeamR tid)

    -- These pages never call breadcrumb
    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb FaviconR = return ("", Nothing)
    breadcrumb RobotsR = return ("", Nothing)
    breadcrumb LocationsR = return ("", Nothing)
    breadcrumb DeleteAccountR = return ("", Nothing)
    breadcrumb SkillsR = return ("", Nothing)
    breadcrumb DeleteIdentR{} = return ("", Nothing)
    breadcrumb RequestRealR = return ("", Nothing)
    breadcrumb RequestRealPicR = return ("", Nothing)
    breadcrumb RequestUnblockR = return ("", Nothing)
    breadcrumb RequestSkillR = return ("", Nothing)
    breadcrumb SetUsernameR = return ("", Nothing)
    breadcrumb ClearUsernameR = return ("", Nothing)
    breadcrumb PackagesR = return ("", Nothing)
    breadcrumb DeletePackageR{} = return ("", Nothing)
    breadcrumb AdminR{} = return ("", Nothing)
    breadcrumb UnadminR{} = return ("", Nothing)
    breadcrumb RealR{} = return ("", Nothing)
    breadcrumb UnrealR{} = return ("", Nothing)
    breadcrumb RealPicR{} = return ("", Nothing)
    breadcrumb UnrealPicR{} = return ("", Nothing)
    breadcrumb BlockR{} = return ("", Nothing)
    breadcrumb UnblockR{} = return ("", Nothing)
    breadcrumb ByIdentR = return ("", Nothing)
    breadcrumb ResetEmailR = return ("", Nothing)
    breadcrumb SendVerifyR = return ("", Nothing)
    breadcrumb CloseMessageR{} = return ("", Nothing)
    breadcrumb NewsFeedR = return ("", Nothing)
    breadcrumb JobsFeedR = return ("", Nothing)
    breadcrumb AuthR{} = return ("", Nothing)
    breadcrumb ScreenNamesR = return ("", Nothing)
    breadcrumb DeleteScreenNameR{} = return ("", Nothing)
    breadcrumb TeamFeedR{} = return ("", Nothing)
    breadcrumb UserFeedR{} = return ("", Nothing)
    breadcrumb TeamNewsR{} = return ("", Nothing)
    breadcrumb LeaveTeamR{} = return ("", Nothing)
    breadcrumb WatchTeamR{} = return ("", Nothing)
    breadcrumb JoinTeamR{} = return ("", Nothing)
    breadcrumb ApproveTeamR{} = return ("", Nothing)
    breadcrumb TeamAdminR{} = return ("", Nothing)
    breadcrumb TeamUnadminR{} = return ("", Nothing)
    breadcrumb DeleteTeamPackageR{} = return ("", Nothing)

-- How to run database actions.
instance YesodPersist Haskellers where
    type YesodDB Haskellers = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodJquery Haskellers where
    urlJqueryUiCss _ = Left $ StaticR jquery_ui_css
instance YesodNic Haskellers

instance YesodAuth Haskellers where
    type AuthId Haskellers = UserId

    loginDest _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = do
        muid <- maybeAuth
        x <- debugRunDB $ getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (_, i), Nothing) -> return $ Just $ identUser i
            (Nothing, Nothing) -> debugRunDB $ do
                uid <- insert $ User
                    { userFullName = ""
                    , userWebsite = Nothing
                    , userEmail = Nothing
                    , userVerifiedEmail = False
                    , userVerkey = Nothing
                    , userHaskellSince = Nothing
                    , userDesc = Nothing
                    , userVisible = False
                    , userReal = False
                    , userRealPic = False
                    , userAdmin = False
                    , userEmployment = Nothing
                    , userBlocked = False
                    , userEmailPublic = False
                    , userLocation = Nothing
                    , userLongitude = Nothing
                    , userLatitude = Nothing
                    }
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid
            (Nothing, Just (uid, _)) -> do
                setMessage $ string "Identifier added to your account"
                _ <- debugRunDB $ insert $ Ident (credsIdent creds) uid
                return $ Just uid
            (Just _, Just _) -> do
                setMessage $ string "That identifier is already attached to an account. Please detach it from the other account first."
                redirect RedirectTemporary ProfileR

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral
    authPlugins = [ authOpenId
                  , authFacebook "157813777573244"
                                 "327e6242e855954b16f9395399164eec"
                                 []
                  ]

    loginHandler = defaultLayout $ do
        [$hamlet|
!style="width:500px;margin:0 auto" ^login^
|]

login :: GWidget s Haskellers ()
login = addCassius $(cassiusFile "login") >> $(hamletFile "login")

userR :: ((UserId, User), Maybe Username) -> HaskellersRoute
userR (_, Just (Username _ un)) = UserR un
userR ((uid, _), _) = UserR $ showIntegral uid

debugInfo :: TVar (Map.Map (String, Int) (Int, Int))
debugInfo = unsafePerformIO $ newTVarIO Map.empty

getDebugR :: Handler RepHtml
getDebugR = do
    l <- Map.toList `fmap` liftIO (atomically $ readTVar debugInfo)
    defaultLayout $ do
        [$hamlet|
%table
    %thead
        %tr
            %th File
            %th Line
            %th Start
            %th Stop
    %tbody
        $forall l p
            %tr
                %td $fst.fst.p$
                %td $show.snd.fst.p$
                %td $show.fst.snd.p$
                %td $show.snd.snd.p$
|]

debugRunDBInner
    :: YesodPersist m
    => String
    -> Int
    -> YesodDB m (GHandler s m) a
    -> GHandler s m a
debugRunDBInner file line db = do
    liftIO addStart
    finally (runDB db) (liftIO addStop)
  where
    addStart = atomically $ do
        m <- readTVar debugInfo
        let (start, stop) = fromMaybe (0, 0) $ Map.lookup (file, line) m
        writeTVar debugInfo $ Map.insert (file, line) (start + 1, stop) m
    addStop = atomically $ do
        m <- readTVar debugInfo
        let (start, stop) = fromMaybe (0, 0) $ Map.lookup (file, line) m -- maybe should never happen
        writeTVar debugInfo $ Map.insert (file, line) (start, stop + 1) m

prettyTime :: UTCTime -> String
prettyTime = formatTime defaultTimeLocale "%B %e, %Y %r"

prettyDay :: Day -> String
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"

addTeamNews :: TeamId -> String -> Html -> HaskellersRoute -> SqlPersist (GHandler Haskellers Haskellers) ()
addTeamNews tid title content url = do
    render <- lift getUrlRender
    now <- liftIO getCurrentTime
    _ <- insert $ TeamNews tid now title content $ render url
    return ()
