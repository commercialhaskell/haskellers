module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , maybeAuth'
    , requireAuth'
    , module Settings
    , module Model
    , login
    , Profile (..)
    , Location (..)
    , userR
    , getDebugR
    , prettyTime
    , prettyDay
    , addTeamNews
    , humanReadableTimeDiff
    , userFullName
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId hiding (forwardUrl)
import Yesod.Auth.Dummy
import Yesod.Auth.OpenId
import Yesod.Auth.Facebook.ServerSide
import qualified Yesod.Auth.GoogleEmail2 as Google
import Facebook (Credentials (Credentials))
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import Control.Monad (unless)
import Data.Char (isSpace)
import qualified Settings
import qualified Database.Persist
import Settings.StaticFiles
import Database.Persist.Sql
import Settings (widgetFile, Extra (..))
import Model hiding (userFullName)
import qualified Model (userFullName)
import Text.Jasmine (minifym)
import Text.Hamlet
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.HTTP.Types (encodePath, queryTextToQuery)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.Set as Set
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Control.Concurrent.STM
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.IORef (IORef)
import Yesod.Facebook
import Network.Mail.Mime.SES (SES)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , homepageProfiles :: IORef ([Profile], Int)
    , publicProfiles :: IORef [Profile]
    , sesCreds :: Text -> SES
    , appGoogleEmailCreds :: (Text, Text)
    }

data Location = Location
    { locationLong :: Double
    , locationLat :: Double
    }
  deriving Show

data Profile = Profile
    { profileUserId :: UserId
    , profileName :: Text
    , profileEmail :: Text
    , profileUser :: User
    , profileSkills :: Set.Set SkillId
    , profileUsername :: Maybe Username
    , profileLocation :: Maybe Location
    }
  deriving Show

prettyTime :: UTCTime -> String
prettyTime = formatTime defaultTimeLocale "%B %e, %Y %r"

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Handler (FormResult x, Widget)

maybeAuth' :: Handler (Maybe ((UserId, User), Maybe Username))
maybeAuth' = do
    x <- maybeAuth
    case x of
        Nothing -> return Nothing
        Just (Entity uid u) -> do
            y <- runDB $ getBy $ UniqueUsernameUser uid
            return $ Just ((uid, u), fmap entityVal y)

requireAuth' :: Handler ((UserId, User), Maybe Username)
requireAuth' = do
    Entity uid u <- requireAuth
    y <- runDB $ getBy $ UniqueUsernameUser uid
    return ((uid, u), fmap entityVal y)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    joinPath _ ar pieces qs' =
        fromText ar
        `mappend` encodePath pieces' (queryTextToQuery qs)
      where
        qs = map (\(x, y) -> (x, if T.null y then Nothing else Just y)) qs'
        pieces'
            | pieces == ["auth", "page", "openid", "complete"] = ["auth", "page", "openid", "complete", ""] -- For Google, it remembers the old OpenIDs
            | otherwise = pieces
    cleanPath _ ["auth", "page", "openid", "complete", ""] = Right ["auth", "page", "openid", "complete"]
    cleanPath _ s =
        if corrected == s
            then Right s
            else Left corrected
      where
        corrected = filter (not . T.null) s

    approot = ApprootMaster $ appRoot . settings

    defaultLayout widget = do
        mmsg <- getMessage
        ma <- maybeAuth'
        y <- getYesod
        (title', parents) <- breadcrumbs
        current <- getCurrentRoute
        let bodyClass =
                case current of
                    Just UsersR -> "find-haskeller"
                    Just UserR{} -> "find-haskeller"
                    Just JobsR -> "find-job"
                    Just JobR{} -> "find-job"
                    Just TeamsR -> "teams"
                    Just TeamR{} -> "teams"
                    Just TopicsR{} -> "teams"
                    Just TopicR{} -> "teams"
                    _ -> "overview" :: T.Text
        let title = if current == Just RootR
                        then "Haskellers"
                        else title'
        pc <- widgetToPageContent $ do
            case ma of
                Nothing -> return ()
                Just ((uid, _), _) -> toWidgetHead [hamlet|<link href="@{UserFeedR uid}" type="application/atom+xml" rel="alternate" title="Your App Updates">
|]
            toWidget $(Settings.cassiusFile "templates/default-layout.cassius")
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            toWidget $(Settings.juliusFile "templates/analytics.julius")
            toWidget $(Settings.juliusFile "templates/default-layout.julius")
            addScriptRemote "https://browserid.org/include.js"
            widget
        let login' = $(ihamletFile "templates/login.hamlet")
        let langs :: [(Text, Text)]
            langs =
                [ ("en", "English")
                , ("ja", "Japanese")
                , ("es", "Spanish")
                , ("fr", "French")
                , ("he", "Hebrew")
                , ("ru", "Russian")
                , ("ua", "Ukrainian")
                ]
        ihamletToHtml $(ihamletFile "templates/default-layout.hamlet")

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend 120 "config/db/client-session-key.aes"

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

instance YesodBreadcrumbs App where
    breadcrumb RootR = return ("Homepage", Nothing)
    breadcrumb FaqR = return ("Frequently Asked Questions", Just RootR)
    breadcrumb BlingR = return ("Bling", Just RootR)
    breadcrumb NewsR = return ("News Archive", Just RootR)
    breadcrumb (NewsItemR nid) = do
        n <- runDB $ get404 nid
        return (newsTitle n, Just NewsR)
    breadcrumb UsersR = return ("Browse Users", Just RootR)
    breadcrumb AllSkillsR = return ("Browse Skills", Just RootR)
    breadcrumb (SkillR sid) = do
        s <- runDB $ get404 sid
        return (skillName s, Just AllSkillsR)
    breadcrumb (FlagR uid) = return ("Report a User", Just $ UserR $ toPathPiece uid)
    breadcrumb (UserR str) = do
        u <- runDB $
            case Data.Text.Read.decimal str :: Either String (Int, Text) of
                Right (_, "") -> get404 $ fromJust $ fromPathPiece str
                _ -> do
                    x <- getBy $ UniqueUsername str
                    case x of
                        Nothing -> lift notFound
                        Just (Entity _ un) -> get404 $ usernameUser un
        return (Foundation.userFullName u, Nothing)
    breadcrumb ProfileR = return ("Edit Your Profile", Just RootR)
    breadcrumb VerifyEmailR{} = return ("Verify Your Email Address", Nothing)
    breadcrumb AdminUsersR = return ("User List- Admin", Nothing)
    breadcrumb MessagesR = return ("Messages- Admin", Nothing)
    breadcrumb (AuthR LoginR) = return ("Log in to Haskellers", Just RootR)
    breadcrumb DebugR = return ("Database pool debug info", Just RootR)
    breadcrumb PollsR = return ("Polls", Just RootR)
    breadcrumb (PollR pollid) = do
        poll <- runDB $ get404 pollid
        return (pollQuestion poll, Just PollsR)

    breadcrumb JobsR = return ("Job Listings", Just RootR)
    breadcrumb (JobR jid) = do
        j <- runDB $ get404 jid
        return (T.concat
            [ jobTitle j
            , " - "
            , T.pack . prettyDay . utctDay . jobPostedAt $ j
            ], Just JobsR
            )
    breadcrumb TeamsR = return ("Special Interest Groups", Just RootR)
    breadcrumb (TeamR tid) = do
        t <- runDB $ get404 tid
        return (teamName t, Just TeamsR)
    breadcrumb (TeamPackagesR tid) = return ("Add Package", Just $ TeamR tid)

    breadcrumb (TopicsR tid) = return ("Discussion Topics", Just $ TeamR tid)
    breadcrumb (TopicR toid) = do
        t <- runDB $ get404 toid
        return (topicTitle t, Just $ TopicsR $ topicTeam t)

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
    breadcrumb TopicMessageR{} = return ("", Nothing)
    breadcrumb LangR{} = return ("", Nothing)
    breadcrumb PollCloseR{} = return ("", Nothing)
    breadcrumb MessagesFeedR{} = return ("", Nothing)
    breadcrumb MessagesFeedLinkR{} = return ("", Nothing)
    breadcrumb CloseJobR{} = return ("", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        master <- getYesod
        Database.Persist.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodJquery App where
    urlJqueryUiCss _ = Left $ StaticR jquery_ui_css
instance YesodNic App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = do
        fixBrowserId creds
        muid <- maybeAuth
        x <- runDB $ do
            x1 <- getBy $ UniqueIdent $ credsIdentClaimed creds
            case x1 of
                Just _ -> return x1
                Nothing -> getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (Entity _ i), Nothing) -> do
                runDB $ do
                    addBIDEmail (identUser i)
                    addClaimed (identUser i) creds
                return $ Just $ identUser i
            (Nothing, Nothing) -> runDB $ do
                uid <- insert $ User
                    { Model.userFullName = ""
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
                    , userGooglePlus = Nothing
                    }
                addBIDEmail uid
                _ <- insert $ Ident (credsIdent creds) uid
                addClaimed uid creds
                return $ Just uid
            (Nothing, Just (Entity uid _)) -> do
                setMessage "Identifier added to your account"
                runDB $ do
                    addBIDEmail uid
                    _ <- insert $ Ident (credsIdent creds) uid
                    addClaimed uid creds
                return $ Just uid
            (Just _, Just (Entity uid _)) -> do
                runDB $ addClaimed uid creds
                setMessage "That identifier is already attached to an account. Please detach it from the other account first."
                redirect ProfileR
      where
        addBIDEmail uid
            | credsPlugin creds == "browserid" = do
                u <- get404 uid
                unless (userVerifiedEmail u) $ update uid [UserEmail =. Just (credsIdent creds), UserVerifiedEmail =. True]
            | otherwise = return ()

        addClaimed uid creds' = do
            let claimed = credsIdentClaimed creds'
            _ <- insertBy $ Ident claimed uid
            return ()

    authPlugins app =
        [ authDummy
        , authOpenId OPLocal []
        , authFacebook []
        , authBrowserId def
        , uncurry Google.authGoogleEmail (appGoogleEmailCreds app)
        ]

    authHttpManager = httpManager

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

    loginHandler = lift $ defaultLayout $ [whamlet|<div style="width:500px;margin:0 auto">^{login}|]

instance YesodAuthPersist App

instance YesodFacebook App where
    fbCredentials _ = Credentials
        "App.com"
        "157813777573244"
        "327e6242e855954b16f9395399164eec"
    fbHttpManager = httpManager

login :: Widget
login = toWidget $ {-addCassius $(cassiusFile "login") >> -}$(hamletFile "templates/login.hamlet")

userR :: ((UserId, User), Maybe Username) -> Route App
userR (_, Just (Username _ un)) = UserR un
userR ((uid, _), _) = UserR $ toPathPiece uid

debugInfo :: TVar (Map.Map (String, Int) (Int, Int))
debugInfo = unsafePerformIO $ newTVarIO Map.empty

getDebugR :: Handler Html
getDebugR = do
    l <- Map.toList `fmap` liftIO (atomically $ readTVar debugInfo)
    defaultLayout $ do
        toWidget [hamlet|\
<table>
    <thead>
        <tr>
            <th>File
            <th>Line
            <th>Start
            <th>Stop
    <tbody>
        $forall p <- l
            <tr>
                <td>#{fst (fst p)}
                <td>#{show (snd (fst p))}
                <td>#{show (fst (snd p))}
                <td>#{show (snd (snd p))}
|]

prettyDay :: Day -> String
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"

addTeamNews :: TeamId -> Text -> Html -> Route App -> SqlPersistT Handler ()
addTeamNews tid title content url = do
    render <- lift getUrlRender
    now <- liftIO getCurrentTime
    _ <- insert $ TeamNews tid now title content $ render url
    return ()

humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

    minutes :: NominalDiffTime -> Double
    minutes n = realToFrac $ n / 60

    hours :: NominalDiffTime -> Double
    hours   n = (minutes n) / 60

    days :: NominalDiffTime -> Double
    days    n = (hours n) / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = (days n) / 7

    years :: NominalDiffTime -> Double
    years   n = (days n) / 365

    i2s :: RealFrac a => a -> String
    i2s n = show m where m = truncate n :: Int

    old = utcToLocalTime utc oldTime

    trim = f . f where f = reverse . dropWhile isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper  d | d < 1          = "one second ago"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 = i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = i2s (hours d) ++ " hours ago"
              | days d < 5     = dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = thisYear
              | otherwise      = previousYears

userFullName :: User -> Text
userFullName =
    go . Model.userFullName
  where
    go "" = "<Unnamed user>"
    go x = x

browserIdDest :: AuthRoute
browserIdDest = PluginR "browserid" []

fixBrowserId :: Creds App -> Handler ()
fixBrowserId creds
    | credsPlugin creds == "browserid" = runDB $ do
        liftIO $ putStrLn "here i am"
        let email = credsIdent creds
        x <- getBy $ UniqueIdent email
        case x of
            Just _ -> return ()
            Nothing -> do
                mu <- selectList [UserEmail ==. Just email, UserVerifiedEmail ==. True] [LimitTo 1]
                case mu of
                    [Entity uid _] -> do
                        _ <- insert $ Ident email uid
                        return ()
                    _ -> return ()
    | otherwise = return ()
