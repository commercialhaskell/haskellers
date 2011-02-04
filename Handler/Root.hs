{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.Root
    ( getRootR
    , getUsersR
    , gravatar
    , getLocationsR
    , yearField
    ) where

import Haskellers hiding (Filter)
import qualified Model
import Yesod.Form.Core
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace, isMark)
import Data.Maybe (fromMaybe)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Data.IORef (readIORef)
import Control.Applicative
import Yesod.Form.Jquery
import Data.Time
import qualified Data.Text as T
import Data.Text.ICU.Normalize

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
    news <- runDB $ selectList [] [NewsWhenDesc] 1 0
    now <- liftIO getCurrentTime
    let minus24h = addUTCTime ((-1) * 60 * 60 * 24) now
    job <- runDB $ selectList [JobPostedAtGt minus24h]
                                   [JobPostedAtDesc] 1 0
    let profs =
            if null allProfs
                then []
                else take 24 $ shuffle' allProfs len gen
    mu <- maybeAuth
    let fuzzyDiffTime = humanReadableTimeDiff now
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
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addCassius $(cassiusFile "jobs")
        addCassius $(cassiusFile "users")
        $(widgetFile "homepage")

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
    norm = T.filter validChar . T.map toLower . normalize NFKD . T.pack
    validChar = not . isMark
    isInfixOf x y = norm x `T.isInfixOf` norm y
    name x = x `isInfixOf` profileName p
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

filterForm :: Int -> Form s y Filter
filterForm my = fieldsToTable $ Filter
    <$> maybeStringField "Name" Nothing
    <*> yearField 1980 my "Started using Haskell no earlier than" Nothing
    <*> yearField 1980 my "Started using Haskell no later than" Nothing
    <*> boolField "Interested in full-time positions" Nothing
    <*> boolField "Interested in part-time positions" Nothing

yearField :: Int -> Int -> FormFieldSettings -> FormletField s m (Maybe Int)
yearField x y = optionalFieldHelper $ yearFieldProfile x y

yearFieldProfile :: Int -> Int -> FieldProfile sub y Int
yearFieldProfile minY maxY = FieldProfile
    { fpParse = \s ->
        case readIntegral s of
            Nothing -> Left "Invalid integer"
            Just i
                | i < minY -> Left $ "Value must be at least " ++ show minY
                | i > maxY -> Left $ "Value must be at most " ++ show maxY
                | otherwise -> Right i
    , fpRender = showIntegral
    , fpWidget = \theId name val isReq -> addHamlet [$hamlet|\
<input id="#{theId}" name="#{name}" type="number" min="#{show minY}" max="#{show maxY}" step="1" :isReq:required="" value="#{val}">
|]
    }

getUsersR :: Handler RepHtmlJson
getUsersR = do
    y <- getYesod
    allProfs <- liftIO $ readIORef $ publicProfiles y
    now <- liftIO getCurrentTime
    let (maxY, _, _) = toGregorian $ utctDay now
    (res, form, enctype) <- runFormGet $ filterForm $ fromInteger maxY
    let filteredProfs =
            case res of
                FormSuccess filt -> filter (applyFilter filt) allProfs
                _ -> allProfs
    let public = length filteredProfs
    mpage <- runFormGet' $ maybeIntInput "page"
    let page = fromMaybe 0 mpage
    let perPage = 32
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
        $(widgetFile "users")
  where
    json r users = jsonMap [("users", jsonList $ map (json' r) users)]
    json' r prof = jsonMap
        [ ("id", jsonScalar $ showIntegral $ profileUserId prof)
        , ("name", jsonScalar $ userFullName $ profileUser prof)
        , ("url", jsonScalar $ r $ profileUserR prof)
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
    cacheSeconds 3600
    jsonToRepJson $ jsonMap [("locations", jsonList $ map (go render) users)]
  where
    go r (uid, u@User
                { userLongitude = Just lng
                , userLatitude = Just lat
                , Model.userFullName = n
                }) = jsonMap
        [ ("lng", jsonScalar $ show lng)
        , ("lat", jsonScalar $ show lat)
        , ("name", jsonScalar n)
        , ("url", jsonScalar $ r $ userR ((uid, u), Nothing))
        ]
    go _ _ = error "getLocationsR"

profileUserR :: Profile -> HaskellersRoute
profileUserR p = userR ((profileUserId p, profileUser p), profileUsername p)
