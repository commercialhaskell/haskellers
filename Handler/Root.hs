{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Haskellers
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Haskellers.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtmlJson
getRootR = do
    mu <- maybeAuth
    users <- runDB $ selectList [ UserVerifiedEmailEq True
                                , UserVisibleEq True
                                ] [UserFullNameAsc] 0 0
    render <- getUrlRender
    flip defaultLayoutJson (json render users) $ do
        setTitle "Haskellers"
        addStyle $(cassiusFile "homepage")
        $(hamletFile "homepage")
  where
    fakeEmail = "fake@email.com"
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
