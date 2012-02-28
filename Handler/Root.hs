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

import Foundation hiding (Filter)
import qualified Model
import Yesod.Form
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
import Data.Text (Text, pack, unpack)

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
    news <- runDB $ selectList [] [Desc NewsWhen, LimitTo 1]
    now <- liftIO getCurrentTime
    let minus24h = addUTCTime ((-1) * 60 * 60 * 24) now
    job <- runDB $ selectList [JobPostedAt >. minus24h]
                                   [Desc JobPostedAt, LimitTo 1]
    let profs =
            if null allProfs
                then []
                else take 24 $ shuffle' allProfs len gen
    mu <- maybeAuth
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
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addCassius $(cassiusFile "jobs")
        addCassius $(cassiusFile "users")
        $(widgetFile "homepage")

data Filter = Filter
    { filterName :: Maybe T.Text
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

filterForm :: Int -> Html -> MForm Haskellers Haskellers (FormResult Filter, Widget)
filterForm my = renderTable $ Filter
    <$> aopt textField "Name" Nothing
    <*> aopt (yearField 1980 my) "Started using Haskell no earlier than" Nothing
    <*> aopt (yearField 1980 my) "Started using Haskell no later than" Nothing
    <*> areq boolField "Must be interested in full-time positions" (Just False)
    <*> areq boolField "Must be interested in part-time positions" (Just False)

yearField :: Int -> Int -> Field sub master Int
yearField minY maxY = Field
    { fieldParse = \ss -> return $
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
    }

getUsersR :: Handler RepHtmlJson
getUsersR = do
    y <- getYesod
    allProfs <- liftIO $ readIORef $ publicProfiles y
    now <- liftIO getCurrentTime
    let (maxY, _, _) = toGregorian $ utctDay now
    ((res, form), enctype) <- runFormGet $ filterForm $ fromInteger maxY
    let filteredProfs =
            case res of
                FormSuccess filt -> filter (applyFilter filt) allProfs
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
    flip defaultLayoutJson (json render profs) $ do
        setTitle "Browsing Haskellers"
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
    [ "http://www.gravatar.com/avatar/"
    , hash
    , "?d=identicon&s="
    , pack $ show s
    ]
  where
    hash = pack $ show $ md5 $ L.fromString $ map toLower $ trim $ unpack x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getLocationsR :: Handler RepJson
getLocationsR = do
    render <- getUrlRender
    users <- runDB $ selectList [ UserLongitude !=. Nothing
                                , UserLatitude !=. Nothing
                                , UserVerifiedEmail ==. True
                                , UserVisible ==. True
                                , UserBlocked ==. False
                                ] []
    cacheSeconds 3600
    jsonToRepJson $ object
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

profileUserR :: Profile -> Route Haskellers
profileUserR p = userR ((profileUserId p, profileUser p), profileUsername p)

postLangR :: Handler ()
postLangR = do
    runInputPost (ireq textField "lang") >>= setLanguage
    md <- runInputPost $ iopt textField "dest"
    case md of
        Nothing -> redirect RootR
        Just d -> redirect d
