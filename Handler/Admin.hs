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
    , getMessagesFeedR
    , getMessagesFeedLinkR
    , postCloseMessageR
    , getAdminUsersR
    , requireAdmin
    ) where

import Import
import Control.Monad (unless)
import Handler.User (adminControls) -- FIXME includes style too many times
import Handler.Root (gravatar)
import Yesod.Form.Jquery (urlJqueryJs)
import Yesod.Feed
import Data.Time (getCurrentTime)

requireAdmin :: Handler ()
requireAdmin = do
    Entity _ admin <- requireAuth
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
postUnrealPicR = adminHelper UserRealPic False "User no longer has real picture status"

postBlockR :: UserId -> Handler ()
postBlockR = adminHelper UserBlocked True "User has been blocked"

postUnblockR :: UserId -> Handler ()
postUnblockR = adminHelper UserBlocked False "User has been unblocked"

getMessagesR :: Handler Html
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
        $(widgetFile "messages")

getMessagesFeedR :: Handler TypedContent
getMessagesFeedR = do
    messages <- runDB $ selectList [MessageClosed ==. False] [Desc MessageWhen, LimitTo 10]
    updated <-
        case messages of
            [] -> liftIO getCurrentTime
            Entity _ m:_ -> return $ messageWhen m

    newsFeed Feed
        { feedTitle = "Haskellers admin messages"
        , feedLinkSelf = MessagesFeedR
        , feedLinkHome = RootR
        , feedAuthor = "Michael Snoyman"
        , feedDescription = "Admin messages for Haskellers.com"
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = map toEntry messages
        }
  where
    toEntry (Entity mid m) = FeedEntry
        { feedEntryLink = MessagesFeedLinkR mid
        , feedEntryUpdated = messageWhen m
        , feedEntryTitle = "Some message"
        , feedEntryContent = "Some message"
        }

getMessagesFeedLinkR :: MessageId -> Handler ()
getMessagesFeedLinkR _ = redirect MessagesR

postCloseMessageR :: MessageId -> Handler ()
postCloseMessageR mid = do
    requireAdmin
    runDB $ update mid [MessageClosed =. True]
    setMessage "Message has been closed"
    redirect MessagesR

getAdminUsersR :: Handler Html
getAdminUsersR = do
    users <- runDB $ selectList [UserVerifiedEmail ==. True] [Asc UserFullName]
    y <- getYesod
    defaultLayout $ do
        setTitle "Admin list of users"
        addScriptEither $ urlJqueryJs y
        $(widgetFile "admin-users")
