{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Haskellers
    ( Haskellers (..)
    , HaskellersRoute (..)
    , resourcesHaskellers
    , Handler
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth2
import Yesod.Helpers.Auth2.OpenId
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Yesod.WebRoutes
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile)
import Model
import StaticFiles (logo_png, jquery_ui_css)
import Yesod.Form.Jquery

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Haskellers = Haskellers
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Haskellers Haskellers

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
/profile ProfileR GET POST
/user/#UserId UserR GET
/profile/delete DeleteAccountR POST

/user/#UserId/admin AdminR POST
/user/#UserId/unadmin UnadminR POST

/user/#UserId/real RealR POST
/user/#UserId/unreal UnrealR POST

/profile/reset-email ResetEmailR POST
/profile/send-verify SendVerifyR POST
/profile/verify/#String VerifyEmailR GET
|]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Haskellers where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addStyle $(Settings.cassiusFile "default-layout")
            addJavascript $(Settings.juliusFile "analytics")
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

-- How to run database actions.
instance YesodPersist Haskellers where
    type YesodDB Haskellers = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodJquery Haskellers where
    urlJqueryUiCss _ = Left $ StaticR jquery_ui_css

instance YesodAuth Haskellers where
    type AuthId Haskellers = UserId

    loginDest _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (_, i) -> return $ Just $ identUser i
            Nothing -> do
                uid <- insert $ User
                    { userFullName = credsIdent creds
                    , userWebsite = Nothing
                    , userEmail = Nothing
                    , userVerifiedEmail = False
                    , userVerkey = Nothing
                    , userDesc = Nothing
                    , userVisible = False
                    , userReal = False
                    , userAdmin = False
                    }
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral
    authPlugins = [authOpenId]

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing
