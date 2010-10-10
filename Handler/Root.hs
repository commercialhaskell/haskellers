{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Haskellers
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace)
import Data.Maybe (fromMaybe)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Haskellers.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    (public, private, unver) <- runDB $ do
        public <- count [UserVerifiedEmailEq True, UserVisibleEq True]
        private <- count [UserVerifiedEmailEq True, UserVisibleEq False]
        unverified <- count [UserVerifiedEmailEq False]
        return (public, private, unverified)
    mpage <- runFormGet' $ maybeIntInput "page"
    let page = fromMaybe 0 mpage
    let perPage = 10
    let hasPrev = page > 0
    let maxPage = (public - 1) `div` perPage
    let hasNext = page < maxPage
    let next = (RootR, [("page", show $ page + 1)])
    let prev = (RootR, [("page", show $ page - 1)])
    let minHaskeller = page * perPage + 1
    users <- runDB $ selectList [ UserVerifiedEmailEq True
                                , UserVisibleEq True
                                ] [UserFullNameAsc] perPage (perPage * page)
    let maxHaskeller = minHaskeller + length users - 1
    defaultLayout $ do
        setTitle "Haskellers"
        addStyle $(cassiusFile "homepage")
        $(hamletFile "homepage")
  where
    fakeEmail = "fake@email.com"

getUsersR :: Handler RepJson
getUsersR = do
    users <- runDB $ selectList [ UserVerifiedEmailEq True
                                , UserVisibleEq True
                                ] [UserFullNameAsc] 0 0
    render <- getUrlRender
    jsonToRepJson $ json render users
  where
    json r users = jsonMap [("users", jsonList $ map (json' r) users)]
    json' r (uid, u) = jsonMap
        [ ("id", jsonScalar $ showIntegral uid)
        , ("name", jsonScalar $ userFullName u)
        , ("url", jsonScalar $ r $ UserR uid)
        ]

gravatar :: Int -> String -> String
gravatar s x =
    "http://www.gravatar.com/avatar/" ++ hash ++ "?d=identicon&s=" ++ show s
  where
    hash = show $ md5 $ L.fromString $ map toLower $ trim x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
