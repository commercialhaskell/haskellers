{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Handler.User
    ( getUserR
    ) where

import Haskellers
import Handler.Root (gravatar)

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
    u <- runDB $ get404 uid
    defaultLayout $(hamletFile "user")
