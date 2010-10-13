{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root
    ( getRootR
    , getUsersR
    , gravatar
    , getLocationsR
    ) where

import Haskellers hiding (Filter)
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace)
import Data.Maybe (fromMaybe)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Data.IORef (readIORef)
import Control.Applicative
import Data.List (isInfixOf)
import Yesod.Form.Jquery

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Haskellers.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    y <- getYesod
    (allProfs, len) <- liftIO $ readIORef $ homepageProfiles y
    gen <- liftIO newStdGen
    let profs = take 9 $ shuffle' allProfs len gen
    mu <- maybeAuth
    (public, private) <- runDB $ do
        public <- count [ UserVerifiedEmailEq True
                        , UserVisibleEq True
                        , UserBlockedEq False
                        ]
        private <- count [ UserVerifiedEmailEq True
                         , UserVisibleEq False
                         , UserBlockedEq False
                         ]
        return (public, private)
    defaultLayout $ do
        setTitle "Haskellers"
        addStyle $(cassiusFile "homepage")
        addStyle $(cassiusFile "users")
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addJavascript $(juliusFile "homepage")
        $(hamletFile "homepage")

data Filter = Filter
    { filterName :: Maybe String
    , filterMinSince :: Maybe Int
    , filterMaxSince :: Maybe Int
    , filterFullTime :: Bool
    , filterPartTime :: Bool
    -- filter for skills
    }

applyFilter :: Filter -> Profile -> Bool
applyFilter f p = and
    [ go name filterName
    , go minsince filterMinSince
    , go maxsince filterMaxSince
    , go fulltime (Just . filterFullTime)
    , go parttime (Just . filterPartTime)
    ]
  where
    go x y =
        case y f of
            Nothing -> True
            Just z -> x z
    name x = map toLower x `isInfixOf` map toLower (profileName p)
    minsince x =
        case userHaskellSince $ profileUser p of
            Nothing -> False
            Just y -> x <= y
    maxsince x =
        case userHaskellSince $ profileUser p of
            Nothing -> False
            Just y -> x >= y
    fulltime False = True
    fulltime True =
        case userEmployment $ profileUser p of
            Just FullTime -> True
            Just FullPartTime -> True
            _ -> False
    parttime False = True
    parttime True =
        case userEmployment $ profileUser p of
            Just PartTime -> True
            Just FullPartTime -> True
            _ -> False

filterForm :: Form s y Filter
filterForm = fieldsToTable $ Filter
    <$> maybeStringField "Name" Nothing
    <*> maybeIntField "Using Haskell since (minimum)" Nothing
    <*> maybeIntField "Using Haskell since (maximum)" Nothing
    <*> boolField "Interested in full-time positions" Nothing
    <*> boolField "Interested in part-time positions" Nothing

getUsersR :: Handler RepHtmlJson
getUsersR = do
    y <- getYesod
    allProfs <- liftIO $ readIORef $ publicProfiles y
    (res, form, enctype) <- runFormGet filterForm
    let filteredProfs =
            case res of
                FormSuccess filt -> filter (applyFilter filt) allProfs
                _ -> allProfs
    let public = length filteredProfs
    mpage <- runFormGet' $ maybeIntInput "page"
    let page = fromMaybe 0 mpage
    let perPage = 20
    let hasPrev = page > 0
    let maxPage = (public - 1) `div` perPage
    let hasNext = page < maxPage
    allGets <- fmap reqGetParams getRequest
    let params p = ("page", show p) : filter (\(x, _) -> x /= "page") allGets
    let next = (UsersR, params $ page + 1)
    let prev = (UsersR, params $ page - 1)
    let minHaskeller = page * perPage + 1
    let profs = take perPage $ drop (page * perPage) filteredProfs
    let maxHaskeller = minHaskeller + length profs - 1
    let noFilter = (UsersR, [("page", show page)])
    render <- getUrlRender
    flip defaultLayoutJson (json render profs) $ do
        setTitle "Browsing Haskellers"
        addStyle $(cassiusFile "users")
        $(hamletFile "users")
  where
    json r users = jsonMap [("users", jsonList $ map (json' r) users)]
    json' r prof = jsonMap
        [ ("id", jsonScalar $ showIntegral $ profileUserId prof)
        , ("name", jsonScalar $ userFullName $ profileUser prof)
        , ("url", jsonScalar $ r $ UserR $ profileUserId prof)
        ]

gravatar :: Int -> String -> String
gravatar s x =
    "http://www.gravatar.com/avatar/" ++ hash ++ "?d=identicon&s=" ++ show s
  where
    hash = show $ md5 $ L.fromString $ map toLower $ trim x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getLocationsR :: Handler RepJson
getLocationsR = do
    render <- getUrlRender
    users <- runDB $ selectList [ UserLongitudeNe Nothing
                                , UserLatitudeNe Nothing
                                , UserVerifiedEmailEq True
                                , UserVisibleEq True
                                , UserBlockedEq False
                                ] [] 0 0
    -- FIXME cache
    jsonToRepJson $ jsonMap [("locations", jsonList $ map (go render) users)]
  where
    go r (uid, User { userLongitude = Just lng, userLatitude = Just lat, userFullName = n }) = jsonMap
        [ ("lng", jsonScalar $ show lng)
        , ("lat", jsonScalar $ show lat)
        , ("name", jsonScalar n)
        , ("url", jsonScalar $ r $ UserR uid)
        ]
    go _ _ = error "getLocationsR"
