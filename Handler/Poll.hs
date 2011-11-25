{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Handler.Poll
    ( getPollsR
    , postPollsR
    , getPollR
    , postPollR
    ) where

import Haskellers
import Control.Monad (unless)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

getPollsR :: Handler RepHtml
getPollsR = do
    polls <- runDB $ selectList [] [LimitTo 5, Desc PollCreated]
    mu <- maybeAuth
    let isAdmin = maybe False (userAdmin . snd) mu
    defaultLayout $(widgetFile "polls")

postPollsR :: Handler RepHtml
postPollsR = do
    (_, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied "Must be an admin to create a poll"
    t <- runInputPost $ ireq textField "poll"
    let ls = filter (not . T.null) $ T.lines t
    unless (length ls >= 3) $ invalidArgs ["Need at least a question and two answers"]
    let (q:as) = ls
    now <- liftIO getCurrentTime
    pollid <- runDB $ do
        pollid <- insert $ Poll q now
        mapM_ (\(a, i) -> insert $ PollOption pollid a i) $ zip as [1..]
        return pollid
    setMessage "Poll created"
    redirect RedirectTemporary $ PollR pollid

data OptInfo = OptInfo
    { oiAnswer :: T.Text
    , oiCount :: Int
    , oiRealCount :: Int
    }

oiPercent :: Bool -> OptInfo -> [OptInfo] -> Maybe Int
oiPercent real oi ois
    | total == 0 = Nothing
    | otherwise = Just $ (f oi * 100) `div` total
  where
    total = sum $ map f ois
    f = if real then oiRealCount else oiCount

toOI :: (PollOptionId, PollOption) -> YesodDB Haskellers Haskellers OptInfo
toOI (poid, po) = do
    x <- count [PollAnswerOption ==. poid]
    y <- count [PollAnswerOption ==. poid, PollAnswerReal ==. True]
    return $ OptInfo (pollOptionAnswer po) x y

getPollR :: PollId -> Handler RepHtml
getPollR pollid = do
    muid <- maybeAuthId
    (poll, ois, options, manswer) <- runDB $ do
        poll <- get404 pollid
        options <- selectList [PollOptionPoll ==. pollid] [Asc PollOptionPriority]
        ois <- mapM toOI options
        manswer <-
            case muid of
                Nothing -> return Nothing
                Just uid -> do
                    ma <- getBy $ UniquePollAnswer pollid uid
                    case ma of
                        Nothing -> return Nothing
                        Just (_, pa) -> do
                            po <- get404 $ pollAnswerOption pa
                            return $ Just $ pollOptionAnswer po
        return (poll, ois, options, manswer)
    defaultLayout $(widgetFile "poll")

postPollR :: PollId -> Handler RepHtml
postPollR pollid = do
    (uid, u) <- requireAuth
    oidText <- runInputPost $ ireq textField "option"
    oid <-
        case fromSinglePiece oidText of
            Nothing -> invalidArgs ["Invalid selection"]
            Just x -> return x
    o <- runDB $ get404 oid
    unless (pollOptionPoll o == pollid) $ invalidArgs ["Poll mismatch"]
    res <- runDB $ insertBy $ PollAnswer pollid oid uid (userReal u)
    setMessage $ either (const "You already voted") (const "Vote cast") res
    redirect RedirectTemporary $ PollR pollid
