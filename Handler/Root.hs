{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.Root
    ( getRootR
    , getUsersR
    , gravatar
    , getLocationsR
    , yearField
    , postLangR
    ) where

import Import hiding (Filter)
import qualified Model
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace, isMark)
import Data.Maybe (fromMaybe)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Data.IORef (readIORef)
import Yesod.Form.Jquery
import Data.Time
import qualified Data.Text as T
import Data.Text.ICU.Normalize
import Data.Text (pack, unpack)
import Data.List (sortBy)
import Data.Ord (comparing)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Haskellers.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Html
getRootR = do
    y <- getYesod
    (allProfs, len) <- liftIO $ readIORef $ homepageProfiles y
    gen <- liftIO newStdGen
    news <- runDB $ selectList [] [Desc NewsWhen, LimitTo 1]
    now <- liftIO getCurrentTime
    let minus7d = addUTCTime ((-1) * 60 * 60 * 24 * 7) now
    job <- runDB $ selectList [JobPostedAt >. minus7d, JobOpen ==. True]
                                   [Desc JobPostedAt, LimitTo 1]
    let profs =
            if null allProfs
                then []
                else take 24 $ shuffle' allProfs len gen
    let fuzzyDiffTime = humanReadableTimeDiff now
    (public, private) <- runDB $ do
        public <- count [ UserVerifiedEmail ==. True
                        , UserVisible ==. True
                        , UserBlocked ==. False
                        ]
        private <- count [ UserVerifiedEmail ==. True
                         , UserVisible ==. False
                         , UserBlocked ==. False
                         ]
        return (public, private)
    defaultLayout $ do
        setTitle "Haskellers"
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
        addScriptRemote "https://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/src/markerclusterer.js"
        toWidget $(cassiusFile "templates/jobs.cassius")
        toWidget $(cassiusFile "templates/users.cassius")
        $(widgetFile "homepage")

data Filter = Filter
    { filterName :: Maybe T.Text
    , filterMinSince :: Maybe Int
    , filterMaxSince :: Maybe Int
    , filterFullTime :: Bool
    , filterPartTime :: Bool
    -- filter for skills
    , filterLocation :: Maybe Location
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
    go :: (z -> Bool) -> (Filter -> Maybe z) -> Bool
    go x y =
        case y f of
            Nothing -> True
            Just z -> x z
    norm = T.filter validChar . T.map toLower . normalize NFKD
    validChar = not . isMark
    name x = norm x `T.isInfixOf` norm (profileName p)
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

filterForm :: Int -> Form Filter
filterForm my = renderTable $ (\a b c d e f g _ -> Filter a b c d e $ Location <$> f <*> g)
    <$> aopt textField "Name" Nothing
    <*> aopt (yearField 1980 my) "Started using Haskell no earlier than" Nothing
    <*> aopt (yearField 1980 my) "Started using Haskell no later than" Nothing
    <*> areq boolField "Must be interested in full-time positions" (Just False)
    <*> areq boolField "Must be interested in part-time positions" (Just False)
    <*> aopt doubleField "Longitude" { fsId = Just "longitude" } Nothing
    <*> aopt doubleField "Latitude" { fsId = Just "latitude" } Nothing
    <*> aopt textField "Order by proximity to:" { fsId = Just "location" } Nothing

yearField :: Int -> Int -> Field Handler Int
yearField minY maxY = Field
    { fieldParse = \ss _ -> return $
        case ss of
            [""] -> Right Nothing
            [s] ->
                case readIntegral $ T.unpack s of
                    Nothing -> Left "Invalid integer"
                    Just i
                        | i < minY -> Left $ SomeMessage $ T.pack $ "Value must be at least " ++ show minY
                        | i > maxY -> Left $ SomeMessage $ T.pack $ "Value must be at most " ++ show maxY
                        | otherwise -> Right $ Just i
            _ -> Right Nothing
    , fieldView = \theId name _ eval isReq ->
        let val = either id (T.pack . showIntegral) eval
         in toWidget [hamlet|
<input id="#{theId}" name="#{name}" type="number" min="#{show minY}" max="#{show maxY}" step="1" :isReq:required="" value="#{val}">
|]
    , fieldEnctype = UrlEncoded
    }

getUsersR :: Handler TypedContent
getUsersR = do
    y <- getYesod
    allProfs <- liftIO $ readIORef $ publicProfiles y
    now <- liftIO getCurrentTime
    let (maxY, _, _) = toGregorian $ utctDay now
    ((res, form), _enctype) <- runFormGet $ filterForm $ fromInteger maxY
    let filteredProfs =
            case res of
                FormSuccess filt ->
                    let profs' = filter (applyFilter filt) allProfs
                     in case filterLocation filt of
                            Nothing -> profs'
                            Just loc -> map snd $ sortBy (comparing fst) $ map (\p -> (getDistance loc $ profileLocation p, p)) profs'
                _ -> allProfs
    let public = length filteredProfs
    mpage <- runInputGet $ iopt intField "page"
    let page = fromMaybe 0 mpage
    let perPage = 32
    let hasPrev = page > 0
    let maxPage = (public - 1) `div` perPage
    let hasNext = page < maxPage
    allGets <- fmap reqGetParams getRequest
    let params p = ("page", T.pack $ show p) : filter (\(x, _) -> x /= "page") allGets
    let next = (UsersR, params $ page + 1)
    let prev = (UsersR, params $ page - 1)
    let minHaskeller = page * perPage + 1
    let profs = take perPage $ drop (page * perPage) filteredProfs
    let maxHaskeller = minHaskeller + length profs - 1
    let noFilter = (UsersR, [("page", T.pack $ show page)])
    render <- getUrlRender
    flip defaultLayoutJson (return $ json render profs) $ do
        setTitle "Browsing Haskellers"
        addScriptRemote "https://maps.google.com/maps/api/js?sensor=false"
        $(widgetFile "users")
  where
    json r users = object ["users" .= array (map (json' r) users)]
    json' r prof = object
        [ "id"   .= toPathPiece (profileUserId prof)
        , "name" .= userFullName (profileUser prof)
        , "url"  .= r (profileUserR prof)
        ]

gravatar :: Int -> Text -> Text
gravatar s x = T.concat
    [ "https://www.gravatar.com/avatar/"
    , hash
    , "?d=identicon&s="
    , pack $ show s
    ]
  where
    hash = pack $ show $ md5 $ L.fromString $ map toLower $ trim $ unpack x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getLocationsR :: Handler Value
getLocationsR = do
    render <- getUrlRender
    users <- runDB $ selectList [ UserLongitude !=. Nothing
                                , UserLatitude !=. Nothing
                                , UserVerifiedEmail ==. True
                                , UserVisible ==. True
                                , UserBlocked ==. False
                                ] []
    cacheSeconds 3600
    return $ object
        ["locations" .= array (map (go render) users)]
  where
    go r (Entity uid u@User
                { userLongitude = Just lng
                , userLatitude = Just lat
                , Model.userFullName = n
                }) = object
        [ "lng"  .= show lng
        , "lat"  .= show lat
        , "name" .= n
        , "url"  .= r (userR ((uid, u), Nothing))
        ]
    go _ _ = error "getLocationsR"

profileUserR :: Profile -> Route App
profileUserR p = userR ((profileUserId p, profileUser p), profileUsername p)

postLangR :: Handler ()
postLangR = do
    runInputPost (ireq textField "lang") >>= setLanguage
    md <- runInputPost $ iopt textField "dest"
    case md of
        Nothing -> redirect RootR
        Just d -> redirect d

data Distance = Distance Double | Unknown
    deriving (Show, Eq, Ord)

getDistance :: Location -> Maybe Location -> Distance
getDistance _ Nothing = Unknown
getDistance (Location x1 y1) (Just (Location x2 y2)) =
    Distance c
  where
    toRad deg = deg / 180 * pi

    xd = toRad $ x2 - x1
    yd = toRad $ y2 - y1

    z1 = toRad y1
    z2 = toRad y2

    a = sin (yd / 2) * sin (yd / 2)
      + sin (xd / 2) * sin (xd / 2) * cos z1 * cos z2

    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
