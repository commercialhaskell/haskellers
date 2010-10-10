{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Admin
    ( postAdminR
    , postUnadminR
    ) where

import Haskellers
import Control.Monad (unless)

postAdminR :: UserId -> Handler ()
postAdminR uid = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"
    runDB $ update uid [UserAdmin True]
    setMessage "User is now an admin"
    redirect RedirectTemporary $ UserR uid

postUnadminR :: UserId -> Handler ()
postUnadminR uid = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"
    runDB $ update uid [UserAdmin False]
    setMessage "User is no longer an admin"
    redirect RedirectTemporary $ UserR uid
