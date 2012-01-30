{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Handler.Admin
    ( postAdminR
    , postUnadminR
    , postRealR
    , postUnrealR
    , postRealPicR
    , postUnrealPicR
    , postBlockR
    , postUnblockR
    , getMessagesR
    , postCloseMessageR
    , getAdminUsersR
    , requireAdmin
    ) where

import Foundation
import Control.Monad (unless)
import Handler.User (adminControls) -- FIXME includes style too many times
import Handler.Root (gravatar)
import Yesod.Form.Jquery (urlJqueryJs)

requireAdmin :: Handler ()
requireAdmin = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"

adminHelper :: EntityField User Bool -> Bool -> Html -> UserId -> Handler ()
adminHelper constr bool msg uid = do
    requireAdmin
    u <- runDB $ get404 uid
    runDB $ update uid [constr =. bool]
    setMessage msg
    redirect $ userR ((uid, u), Nothing)

postAdminR :: UserId -> Handler ()
postAdminR = adminHelper UserAdmin True "User is now an admin"

postUnadminR :: UserId -> Handler ()
postUnadminR = adminHelper UserAdmin False "User is no longer an admin"

postRealR :: UserId -> Handler ()
postRealR = adminHelper UserReal True "User now has verified user status"

postUnrealR :: UserId -> Handler ()
postUnrealR = adminHelper UserReal False "User no longer has verified user status"

postRealPicR :: UserId -> Handler ()
postRealPicR = adminHelper UserRealPic True "User now has real picture status"

postUnrealPicR :: UserId -> Handler ()
postUnrealPicR = adminHelper UserReal False "User no longer has real picture status"

postBlockR :: UserId -> Handler ()
postBlockR = adminHelper UserBlocked True "User has been blocked"

postUnblockR :: UserId -> Handler ()
postUnblockR = adminHelper UserBlocked False "User has been unblocked"

getMessagesR :: Handler RepHtml
getMessagesR = do
    requireAdmin
    messages <- runDB $ selectList [MessageClosed ==. False] [Asc MessageWhen] >>= mapM (\(Entity mid m) -> do
        let go uid = do
                u <- get404 uid
                return $ Just (uid, u)
        from <- maybe (return Nothing) go $ messageFrom m
        regarding <- maybe (return Nothing) go $ messageRegarding m
        return ((mid, m), (from, regarding))
        )
    defaultLayout $ do
        setTitle "Admin Messages"
        addCassius $(cassiusFile "messages")
        addWidget $(hamletFile "messages")

postCloseMessageR :: MessageId -> Handler ()
postCloseMessageR mid = do
    requireAdmin
    runDB $ update mid [MessageClosed =. True]
    setMessage "Message has been closed"
    redirect MessagesR

getAdminUsersR :: Handler RepHtml
getAdminUsersR = do
    users <- runDB $ selectList [UserVerifiedEmail ==. True] [Asc UserFullName]
    y <- getYesod
    defaultLayout $ do
        setTitle "Admin list of users"
        addCassius $(cassiusFile "admin-users")
        addScriptEither $ urlJqueryJs y
        addJulius $(juliusFile "admin-users")
        $(hamletFile "admin-users")
