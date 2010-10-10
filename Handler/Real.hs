{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Real
    ( postRealR
    , postUnrealR
    ) where

import Haskellers
import Control.Monad (unless)

postRealR :: UserId -> Handler ()
postRealR uid = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"
    runDB $ update uid [UserReal True]
    setMessage "User now has the Real Haskeller badge"
    redirect RedirectTemporary $ UserR uid

postUnrealR :: UserId -> Handler ()
postUnrealR uid = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"
    runDB $ update uid [UserReal False]
    setMessage "User no longer has the Real Haskeller badge"
    redirect RedirectTemporary $ UserR uid
