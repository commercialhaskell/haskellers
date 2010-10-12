{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
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
    ) where

import Haskellers
import Control.Monad (unless)
import Handler.User (adminControls) -- FIXME includes style too many times
import Handler.Root (gravatar)
import Yesod.Form.Jquery (urlJqueryJs)

requireAdmin :: Handler ()
requireAdmin = do
    (_, admin) <- requireAuth
    unless (userAdmin admin) $ permissionDenied "You are not an admin"

postAdminR :: UserId -> Handler ()
postAdminR uid = do
    requireAdmin
    runDB $ update uid [UserAdmin True]
    setMessage "User is now an admin"
    redirect RedirectTemporary $ UserR uid

postUnadminR :: UserId -> Handler ()
postUnadminR uid = do
    requireAdmin
    runDB $ update uid [UserAdmin False]
    setMessage "User is no longer an admin"
    redirect RedirectTemporary $ UserR uid

postRealR :: UserId -> Handler ()
postRealR uid = do
    requireAdmin
    runDB $ update uid [UserReal True]
    setMessage "User now has verified user status"
    redirect RedirectTemporary $ UserR uid

postUnrealR :: UserId -> Handler ()
postUnrealR uid = do
    requireAdmin
    runDB $ update uid [UserReal False]
    setMessage "User no longer has verified user status"
    redirect RedirectTemporary $ UserR uid

postRealPicR :: UserId -> Handler ()
postRealPicR uid = do
    requireAdmin
    runDB $ update uid [UserRealPic True]
    setMessage "User now has real picture status"
    redirect RedirectTemporary $ UserR uid

postUnrealPicR :: UserId -> Handler ()
postUnrealPicR uid = do
    requireAdmin
    runDB $ update uid [UserReal False]
    setMessage "User no longer has real picture status"
    redirect RedirectTemporary $ UserR uid

postBlockR :: UserId -> Handler ()
postBlockR uid = do
    requireAdmin
    runDB $ update uid [UserBlocked True]
    setMessage "User has been blocked"
    redirect RedirectTemporary $ UserR uid

postUnblockR :: UserId -> Handler ()
postUnblockR uid = do
    requireAdmin
    runDB $ update uid [UserBlocked False]
    setMessage "User has been unblocked"
    redirect RedirectTemporary $ UserR uid

getMessagesR :: Handler RepHtml
getMessagesR = do
    requireAdmin
    messages <- runDB $ selectList [MessageClosedEq False] [MessageWhenAsc] 0 0 >>= mapM (\(mid, m) -> do
        let go uid = do
                u <- get404 uid
                return $ Just (uid, u)
        from <- maybe (return Nothing) go $ messageFrom m
        regarding <- maybe (return Nothing) go $ messageRegarding m
        return ((mid, m), (from, regarding))
        )
    defaultLayout $ do
        setTitle $ string "Admin Messages"
        addStyle $(cassiusFile "messages")
        $(hamletFile "messages")

postCloseMessageR :: MessageId -> Handler ()
postCloseMessageR mid = do
    requireAdmin
    runDB $ update mid [MessageClosed True]
    setMessage $ string "Message has been closed"
    redirect RedirectTemporary MessagesR

getAdminUsersR :: Handler RepHtml
getAdminUsersR = do
    users <- runDB $ selectList [UserVerifiedEmailEq True] [UserFullNameAsc] 0 0
    y <- getYesod
    defaultLayout $ do
        setTitle $ string "Admin list of users"
        addStyle $(cassiusFile "admin-users")
        addScriptEither $ urlJqueryJs y
        addJavascript $(juliusFile "admin-users")
        $(hamletFile "admin-users")
