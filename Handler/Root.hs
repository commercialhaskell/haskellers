{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Haskellers
import Yesod.Helpers.Auth2 (apLogin)
import Yesod.Helpers.Auth2.OpenId (authOpenId, forwardUrl)
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.Char (toLower, isSpace)
import StaticFiles

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
    users <- runDB $ selectList [ UserHumanEq True
                                , UserVisibleEq True
                                ] [UserFullNameAsc] 0 0
    defaultLayout $ do
        setTitle "Haskellers"
        addStyle $(cassiusFile "homepage")
        $(hamletFile "homepage")
  where
    fakeEmail = "fake@email.com"

gravatar :: Int -> String -> String
gravatar s x =
    "http://www.gravatar.com/avatar/" ++ hash ++ "?d=wavatar&s=" ++ show s
  where
    hash = show $ md5 $ L.fromString $ map toLower $ trim x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
