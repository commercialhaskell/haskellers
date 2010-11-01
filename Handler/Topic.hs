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

topicFormlet :: TeamId -> UserId -> UTCTime -> Form s m Topic
topicFormlet tid uid now = fieldsToTable $ Topic
    <$> pure tid
    <*> pure now
    <*> selectField opts "Topic type" Nothing
    <*> pure Open
    <*> pure (Just uid)
    <*> stringField "Title" Nothing
  where
    opts = map (id &&& show) [minBound..maxBound]

getTopicsR :: TeamId -> Handler RepHtml
getTopicsR tid = do
    topics <- runDB $ selectList [TopicTeamEq tid] [TopicCreatedDesc] 0 0
    ma <- maybeAuth
    mf <-
        case ma of
            Just (uid, User { userVerifiedEmail = True, userBlocked = False }) -> do
                now <- liftIO getCurrentTime
                (form, _, nonce) <- generateForm $ topicFormlet tid uid now
                return $ Just (form, nonce)
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
    (res, form, _, nonce) <- runFormPost $ topicFormlet tid uid now
    case res of
        FormSuccess to -> do
            toid <- runDB $ do
                t <- get404 tid
                toid <- insert to
                addTeamNews tid ("New topic started for " ++ teamName t)
                    [$hamlet|
%p $topicTitle.to$
%p Created by $userFullName.u$. Discussion type: $show.topicType.to$.
|] $ TopicR toid
                return toid
            setMessage "Topic started"
            redirect RedirectTemporary $ TopicR toid
        _ -> defaultLayout [$hamlet|
%form!method=post!action=@TopicsR.tid@
    %table
        ^form^
        %tr
            %td!colspan=2
                $nonce$
                %input!type=submit!value="Create topic"
|]

statusFormlet :: TopicStatus -> Form s m TopicStatus
statusFormlet =
    fieldsToTable . selectField opts "New status" . Just
  where
    opts = map (id &&& show) [minBound..maxBound]

messageForm :: Form s Haskellers Html
messageForm = fieldsToTable $ nicHtmlField "Your message" Nothing

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
    (changeStatus, _, nonce) <- generateForm $ statusFormlet $ topicStatus to
    (form, _, _) <- generateForm messageForm
    mcreator <- case topicCreator to of
                    Nothing -> return Nothing
                    Just uid -> runDB $ get uid
    messages <- runDB $ selectList [TopicMessageTopicEq toid]
                        [TopicMessageCreatedAsc] 0 0
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
    (res, _, _, _) <- runFormPost $ statusFormlet $ topicStatus to
    case res of
        FormSuccess s -> runDB $ do
            update toid [TopicStatus s]
            t <- get404 tid
            lift $ setMessage "Status updated"
            addTeamNews tid ("Topic status update for " ++ teamName t)
                [$hamlet|
%p The status of the topic "$topicTitle.to$" has been updated to $show.s$.
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
    (res, _, _, _) <- runFormPost messageForm
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
        let dest = render (TopicR toid) ++ "#message-" ++ showIntegral mid
        t <- get404 tid
        let title = "Message added for " ++ teamName t
        let content = [$hamlet|
%p $userFullName.u$ wrote regarding $topicTitle.to$
%blockquote $html$
|]
        _ <- insert $ TeamNews tid now title content dest
        return dest
    redirectString RedirectTemporary dest
