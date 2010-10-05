{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Admin
    ( postAdminR
    , postUnadminR
    ) where

import Haskellers

postAdminR :: UserId -> Handler ()
postAdminR = undefined

postUnadminR :: UserId -> Handler ()
postUnadminR = undefined
