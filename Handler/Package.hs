module Handler.Package
    ( postDeletePackageR
    , postPackagesR
    ) where

import Import
import Control.Monad (unless)
import Yesod.Auth (requireAuthId)

postDeletePackageR :: PackageId -> Handler ()
postDeletePackageR pid = do
    uid <- requireAuthId
    p <- runDB $ get404 pid
    unless (packageUser p == uid) notFound
    runDB $ delete pid
    setMessage "Package removed"
    redirect ProfileR

postPackagesR :: Handler ()
postPackagesR = do
    uid <- requireAuthId
    res <- runInputPost $ iopt textField "name"
    case res of
        Just name -> do
            _ <- runDB $ insert $ Package uid name
            setMessage "Package added"
        Nothing -> setMessage "Invalid package name"
    redirect ProfileR
