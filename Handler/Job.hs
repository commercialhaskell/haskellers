{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Job
    ( getJobsR
    , postJobsR
    , getJobR
    , getJobsFeedR
    , postCloseJobR
    ) where

import Import
import Data.Maybe (fromMaybe)
import Data.Time
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Control.Monad (unless)
import Yesod.Feed
import Yesod.Auth

jobFormlet :: UserId -> UTCTime -> Maybe Job -> Form Job
jobFormlet uid now mj = renderTable $ Job
    <$> pure (fromMaybe uid (fmap jobPostedBy mj))
    <*> pure (fromMaybe now (fmap jobPostedAt mj))
    <*> areq textField "Title"
            { fsId = Just "title"
            } (fmap jobTitle mj)
    <*> areq textField "Location"
            { fsTooltip = Just "If this is a telecommuting position, specify here"
            , fsId = Just "location"
            } (fmap jobLocation mj)
    <*> areq (jqueryDayField def) "Filling by" (fmap jobFillingBy mj)
    <*> areq boolField "Full time option?" (fmap jobFullTime mj)
    <*> areq boolField "Part time option?" (fmap jobPartTime mj)
    <*> pure (Textarea "Please see HTML description")
    <*> fmap Just (areq nicHtmlField "Description"
            { fsId = Just "desc"
            } (mj >>= jobDescHtml))
    <*> pure True

getJobsR :: Handler Html
getJobsR = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    let today = utctDay now
    jobs <- runDB $ selectList [JobFillingBy >. today, JobOpen ==. True] [Desc JobPostedAt]
    let isUnverEmail = Just False == fmap (userVerifiedEmail . entityVal) mu
    mform <-
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) ->
                if userVerifiedEmail u
                    then do
                        (form, _) <- generateFormPost $ jobFormlet uid now Nothing
                        return $ Just form
                    else return Nothing
    defaultLayout $ do
        toWidget $(cassiusFile "templates/login-status.cassius")
        $(widgetFile "jobs")

postJobsR :: Handler Html
postJobsR = do
    Entity uid u <- requireAuth
    unless (userVerifiedEmail u) $ permissionDenied "Only users with verified email addresses can add job listings"
    now <- liftIO getCurrentTime
    ((res, form), _) <- runFormPost $ jobFormlet uid now Nothing
    let mform = Just form
    case res of
        FormSuccess job -> do
            jid <- runDB $ insert job
            setMessage "Job posted"
            redirect $ JobR jid
        _ -> return ()
    let jobs = []
    let isUnverEmail = False
    defaultLayout $(widgetFile "jobs")

getJobR :: JobId -> Handler Html
getJobR jid = do
    job <- runDB $ get404 jid
    muid <- maybeAuthId
    let isOwner = Just (jobPostedBy job) == muid
    poster <- runDB $ get404 $ jobPostedBy job
    defaultLayout $(widgetFile "job")

getJobsFeedR :: Handler TypedContent
getJobsFeedR = do
    cacheSeconds 7200
    now <- liftIO getCurrentTime
    let today = utctDay now
    jobs <- runDB $ selectList [JobFillingBy >. today] [Desc JobPostedAt, LimitTo 10]
    let updated =
            case jobs of
                (Entity _ newest):_ -> jobPostedAt newest
                [] -> now
    newsFeed Feed
        { feedTitle = "Haskellers Job Listings"
        , feedLinkSelf = JobsFeedR
        , feedLinkHome = RootR
        , feedUpdated = updated
        , feedEntries = map go jobs
        , feedDescription = "Haskellers Job Listings"
        , feedLanguage = "en"
        , feedAuthor = "Haskellers Job Listings"
        }
  where
    go (Entity jid j) = FeedEntry
        { feedEntryLink = JobR jid
        , feedEntryUpdated = jobPostedAt j
        , feedEntryTitle = jobTitle j
        , feedEntryContent = fromMaybe (toHtml $ jobDesc j) (jobDescHtml j)
        }

postCloseJobR :: JobId -> Handler ()
postCloseJobR jid = do
    uid <- requireAuthId
    j <- runDB $ get404 jid
    if jobPostedBy j == uid
        then do
            runDB $ update jid [JobOpen =. False]
            setMessage "Job posting has been closed"
            redirect $ JobR jid
        else permissionDenied "You did not create this job."
