{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Handler.Root
    ( getRootR
    , getUsersR
    , gravatar
    , getLocationsR
    ) where

#define debugRunDB debugRunDBInner __FILE__ __LINE__

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
import Data.Time
import System.Locale (defaultTimeLocale)

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
    news <- debugRunDB $ selectList [] [NewsWhenDesc] 1 0
    now <- liftIO getCurrentTime
    let minus24h = addUTCTime ((-1) * 60 * 60 * 24) now
    job <- debugRunDB $ selectList [JobPostedAtGt minus24h]
                                   [JobPostedAtDesc] 1 0
    let profs =
            if null allProfs
                then []
                else take 8 $ shuffle' allProfs len gen
    mu <- maybeAuth
    let fuzzyDiffTime = humanReadableTimeDiff now
    (public, private) <- debugRunDB $ do
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
        addCassius $(cassiusFile "homepage")
        addCassius $(cassiusFile "users")
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addJulius $(juliusFile "homepage")
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
        addCassius $(cassiusFile "users")
        $(hamletFile "users")
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
    users <- debugRunDB $ selectList [ UserLongitudeNe Nothing
                                , UserLatitudeNe Nothing
                                , UserVerifiedEmailEq True
                                , UserVisibleEq True
                                , UserBlockedEq False
                                ] [] 0 0
    cacheSeconds 3600
    jsonToRepJson $ jsonMap [("locations", jsonList $ map (go render) users)]
  where
    go r (uid, u@User { userLongitude = Just lng, userLatitude = Just lat, userFullName = n }) = jsonMap
        [ ("lng", jsonScalar $ show lng)
        , ("lat", jsonScalar $ show lat)
        , ("name", jsonScalar n)
        , ("url", jsonScalar $ r $ userR ((uid, u), Nothing))
        ]
    go _ _ = error "getLocationsR"

profileUserR :: Profile -> HaskellersRoute
profileUserR p = userR ((profileUserId p, profileUser p), profileUsername p)

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
