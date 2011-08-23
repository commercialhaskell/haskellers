{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Topic
    ( getTopicsR
    , postTopicsR
    , getTopicR
    , postTopicR
    , postTopicMessageR
    ) where

import Haskellers
import Handler.Team (loginStatus)
import Yesod.Form.Nic
import Data.Time
import Control.Applicative
import Control.Arrow
import Control.Monad (unless)
import qualified Data.Text as T
import Text.Hamlet (shamlet)

topicFormlet :: TeamId -> UserId -> UTCTime -> Html -> Form Haskellers Haskellers (FormResult Topic, Widget)
topicFormlet tid uid now = renderTable $ Topic
    <$> pure tid
    <*> pure now
    <*> areq (selectField opts) "Topic type" Nothing
    <*> pure Open
    <*> pure (Just uid)
    <*> areq textField "Title" Nothing
  where
    opts = map (T.pack . show &&& id) [minBound..maxBound]

getTopicsR :: TeamId -> Handler RepHtml
getTopicsR tid = do
    topics <- runDB $ selectList [TopicTeam ==. tid] [Desc TopicCreated]
    ma <- maybeAuth
    mf <-
        case ma of
            Just (uid, User { userVerifiedEmail = True, userBlocked = False }) -> do
                now <- liftIO getCurrentTime
                ((_, form), _) <- runFormPost $ topicFormlet tid uid now
                return $ Just form
            _ -> return Nothing
    defaultLayout $ do
        addWidget $ loginStatus ma
        addWidget $(hamletFile "topics")
        addCassius $(cassiusFile "topics")

postTopicsR :: TeamId -> Handler RepHtml
postTopicsR tid = do
    (uid, u) <- requireAuth
    unless (userVerifiedEmail u && not (userBlocked u)) $ permissionDenied
        "You must have a verified email address and not be blocked."
    now <- liftIO getCurrentTime
    ((res, form), _) <- runFormPost $ topicFormlet tid uid now
    case res of
        FormSuccess to -> do
            toid <- runDB $ do
                t <- get404 tid
                toid <- insert to
                addTeamNews tid ("New topic started for " `T.append` teamName t)
                    [shamlet|\
<p>#{topicTitle to}
<p>Created by #{userFullName u}. Discussion type: #{show (topicType to)}.
|] $ TopicR toid
                return toid
            setMessage "Topic started"
            redirect RedirectTemporary $ TopicR toid
        _ -> defaultLayout [whamlet|\
<form method="post" action="@{TopicsR tid}">
    <table>
        \^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Create topic">
|]

statusFormlet :: TopicStatus -> Html -> Form Haskellers Haskellers (FormResult TopicStatus, Widget)
statusFormlet =
    renderTable . areq (selectField opts) "New status" . Just
  where
    opts = map (T.pack . show &&& id) [minBound..maxBound]

messageForm :: Html -> Form Haskellers Haskellers (FormResult Html, Widget)
messageForm = renderTable $ areq nicHtmlField "Your message" Nothing

getTopicR :: TopicId -> Handler RepHtml
getTopicR toid = do
    ma <- maybeAuth
    to <- runDB $ get404 toid
    let tid = topicTeam to
    (canMessage, isMember) <-
        case ma of
            Just (uid, User { userVerifiedEmail = True, userBlocked = False }) -> do
                x <- runDB $ getBy $ UniqueTeamUser tid uid
                let im = fmap (teamUserStatus . snd) x `elem`
                           map Just [Admin, ApprovedMember]
                return (True, im)
            _ -> return (False, False)
    ((_, changeStatus), _) <- runFormPost $ statusFormlet $ topicStatus to
    ((_, form), _) <- runFormPost messageForm
    mcreator <- case topicCreator to of
                    Nothing -> return Nothing
                    Just uid -> runDB $ get uid
    messages <- runDB $ selectList [TopicMessageTopic ==. toid]
                        [Asc TopicMessageCreated]
                    >>= mapM (\(mid, m) -> do
        mu <- case topicMessageCreator m of
                Nothing -> return Nothing
                Just c -> get c
        return ((mid, m), mu)
        )
    defaultLayout $ do
        addWidget $ loginStatus ma
        addWidget $(hamletFile "topic")
        addCassius $(cassiusFile "topic")

postTopicR :: TopicId -> Handler ()
postTopicR toid = do
    to <- runDB $ get404 toid
    let tid = topicTeam to
    (uid, u) <- requireAuth
    unless (userVerifiedEmail u && not (userBlocked u)) $ permissionDenied
        "You must have a verified email address and not be blocked."
    x <- runDB $ getBy $ UniqueTeamUser tid uid
    unless (fmap (teamUserStatus . snd) x `elem` map Just [Admin, ApprovedMember]) $ permissionDenied "You must be an approved member."
    ((res, _), _) <- runFormPost $ statusFormlet $ topicStatus to
    case res of
        FormSuccess s -> runDB $ do
            update toid [TopicStatus =. s]
            t <- get404 tid
            lift $ setMessage "Status updated"
            addTeamNews tid ("Topic status update for " `T.append` teamName t)
                [shamlet|\
<p>The status of the topic "#{topicTitle to}" has been updated to #{show s}.
|] $ TopicR toid
        _ -> setMessage "Invalid input"
    redirect RedirectTemporary $ TopicR toid

postTopicMessageR :: TopicId -> Handler RepHtml
postTopicMessageR toid = do
    to <- runDB $ get404 toid
    let tid = topicTeam to
    (uid, u) <- requireAuth
    unless (userVerifiedEmail u && not (userBlocked u)) $ permissionDenied
        "You must have a verified email address and not be blocked."
    ((res, _), _) <- runFormPost messageForm
    html <-
        case res of
            FormSuccess x -> return x
            _ -> do
                setMessage "Invalid message"
                redirect RedirectTemporary $ TopicR toid
    now <- liftIO getCurrentTime
    render <- getUrlRender
    dest <- runDB $ do
        mid <- insert $ TopicMessage toid now (Just uid) html
        let dest = T.concat
                [ render (TopicR toid)
                , "#message-"
                , toSinglePiece mid
                ]
        t <- get404 tid
        let title = "Message added for " `T.append` teamName t
        let content = [shamlet|\
<p>#{userFullName u} wrote regarding #{topicTitle to}
<blockquote>#{html}
|]
        _ <- insert $ TeamNews tid now title content dest
        return dest
    redirectText RedirectTemporary dest
