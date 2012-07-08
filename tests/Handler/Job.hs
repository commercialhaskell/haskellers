{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Job
    ( getJobsR
    , postJobsR
    , getJobR
    , getJobsFeedR
    ) where

import Import
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Time
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Control.Monad (unless)
import Yesod.Feed
import Yesod.Auth

jobFormlet :: UserId -> UTCTime -> Maybe Job -> Html -> MForm Haskellers Haskellers (FormResult Job, Widget)
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

getJobsR :: Handler RepHtml
getJobsR = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    let today = utctDay now
    jobs <- runDB $ selectList [JobFillingBy >. today] [Desc JobPostedAt]
    let isUnverEmail = Just False == fmap (userVerifiedEmail . entityVal) mu
    mform <-
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) ->
                if userVerifiedEmail u
                    then do
                        ((_, form), _) <- runFormGet $ jobFormlet uid now Nothing
                        return $ Just form
                    else return Nothing
    defaultLayout $ do
        addCassius $(cassiusFile "templates/login-status.cassius")
        $(widgetFile "jobs")

postJobsR :: Handler RepHtml
postJobsR = do
    Entity uid u <- requireAuth
    unless (userVerifiedEmail u) $ permissionDenied "Only users with verified email addresses can add job listings"
    now <- liftIO getCurrentTime
    ((res, form), _) <- runFormPostNoToken $ jobFormlet uid now Nothing
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

getJobR :: JobId -> Handler RepHtml
getJobR jid = do
    job <- runDB $ get404 jid
    poster <- runDB $ get404 $ jobPostedBy job
    defaultLayout $(widgetFile "job")

getJobsFeedR :: Handler RepAtomRss
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
        }
  where
    go (Entity jid j) = FeedEntry
        { feedEntryLink = JobR jid
        , feedEntryUpdated = jobPostedAt j
        , feedEntryTitle = jobTitle j
        , feedEntryContent = toHtml $ jobDesc j
        }
