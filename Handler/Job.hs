{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Job
    ( getJobsR
    , postJobsR
    , getJobR
    , getJobsFeedR
    ) where

import Haskellers
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Time
import Yesod.Form.Jquery
import Control.Monad (unless)
import Yesod.Helpers.Feed
import Text.Blaze (toHtml)

jobFormlet :: UserId -> UTCTime -> Formlet s Haskellers Job
jobFormlet uid now mj = fieldsToTable $ Job
    <$> pure (fromMaybe uid (fmap jobPostedBy mj))
    <*> pure (fromMaybe now (fmap jobPostedAt mj))
    <*> stringField "Title"
            { ffsId = Just "title"
            } (fmap jobTitle mj)
    <*> stringField "Location"
            { ffsTooltip = "If this is a telecommuting position, specify here"
            , ffsId = Just "location"
            } (fmap jobLocation mj)
    <*> jqueryDayField def "Filling by" (fmap jobFillingBy mj)
    <*> boolField "Full time option?" (fmap jobFullTime mj)
    <*> boolField "Part time option?" (fmap jobPartTime mj)
    <*> textareaField "Description"
            { ffsId = Just "desc"
            } (fmap jobDesc mj)

getJobsR :: Handler RepHtml
getJobsR = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    let today = utctDay now
    jobs <- runDB $ selectList [JobFillingByGt today] [JobPostedAtDesc] 0 0
    mform <-
        case mu of
            Nothing -> return Nothing
            Just (uid, u) ->
                if userReal u
                    then do
                        (_, form, _) <- runFormGet $ jobFormlet uid now Nothing
                        return $ Just form
                    else return Nothing
    defaultLayout $ do
        addCassius $(cassiusFile "jobs")
        addCassius $(cassiusFile "login-status")
        addWidget $(hamletFile "jobs")

postJobsR :: Handler RepHtml
postJobsR = do
    (uid, u) <- requireAuth
    unless (userReal u) $ permissionDenied "Only verified users can add job listings"
    now <- liftIO getCurrentTime
    (res, form, _) <- runFormPostNoNonce $ jobFormlet uid now Nothing
    let mform = Just form
    case res of
        FormSuccess job -> do
            jid <- runDB $ insert job
            setMessage "Job posted"
            redirect RedirectTemporary $ JobR jid
        _ -> return ()
    let jobs = []
    defaultLayout $ do
        addCassius $(cassiusFile "jobs")
        $(hamletFile "jobs")

getJobR :: JobId -> Handler RepHtml
getJobR jid = do
    job <- runDB $ get404 jid
    poster <- runDB $ get404 $ jobPostedBy job
    defaultLayout $ do
        addCassius $(cassiusFile "job")
        $(hamletFile "job")

getJobsFeedR :: Handler RepAtomRss
getJobsFeedR = do
    cacheSeconds 7200
    now <- liftIO getCurrentTime
    let today = utctDay now
    jobs <- runDB $ selectList [JobFillingByGt today] [JobPostedAtDesc] 10 0
    let updated =
            case jobs of
                (_, newest):_ -> jobPostedAt newest
                [] -> now
    newsFeed Feed
        { feedTitle = "Haskellers Job Listings"
        , feedLinkSelf = JobsFeedR
        , feedLinkHome = RootR
        , feedUpdated = updated
        , feedEntries = map go jobs
        , feedDescription = "Haskellers Job Listings"
        , feedLanguage = "en"
        }
  where
    go (jid, j) = FeedEntry
        { feedEntryLink = JobR jid
        , feedEntryUpdated = jobPostedAt j
        , feedEntryTitle = jobTitle j
        , feedEntryContent = toHtml $ jobDesc j
        }
