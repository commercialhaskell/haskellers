{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Package
    ( postDeletePackageR
    , postPackagesR
    ) where

import Haskellers
import Control.Monad (unless)

postDeletePackageR :: PackageId -> Handler ()
postDeletePackageR pid = do
    (uid, _) <- requireAuth
    p <- runDB $ get404 pid
    unless (packageUser p == uid) notFound
    runDB $ delete pid
    setMessage "Package removed"
    redirect RedirectTemporary ProfileR

postPackagesR :: Handler ()
postPackagesR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPost $ stringInput "name"
    case res of
        FormSuccess name -> do
            _ <- runDB $ insert $ Package uid name
            setMessage "Package added"
        _ -> setMessage "Invalid package name"
    redirect RedirectTemporary ProfileR
