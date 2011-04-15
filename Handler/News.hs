{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.News
    ( getNewsR
    , postNewsR
    , getNewsItemR
    , getNewsFeedR
    ) where

import Haskellers
import Yesod.Helpers.Feed
import Control.Applicative
import Yesod.Form.Nic
import Handler.Admin (requireAdmin)
import Data.Time (getCurrentTime)
import Data.Text (Text)

newsForm :: Form s Haskellers (Text, Html)
newsForm = fieldsToTable $ (,)
    <$> stringField "Title" Nothing
    <*> nicHtmlField "Content"
        { ffsId = Just "content"
        } Nothing

getNewsR :: Handler RepHtml
getNewsR = do
    mu <- maybeAuth
    cacheSeconds 3600
    news <- runDB $ selectList [] [NewsWhenDesc] 0 0
    (_, form, _) <- runFormGet newsForm

    now <- liftIO getCurrentTime
    let fuzzyDiffTime = humanReadableTimeDiff now

    defaultLayout $ do
        setTitle "Haskellers News Archive"
        addWidget $(hamletFile "news")
  where
    newsAdmin = $(cassiusFile "news-admin")

postNewsR :: Handler RepHtml
postNewsR = do
    requireAdmin
    (res, form, _) <- runFormPostNoNonce newsForm
    case res of
        FormSuccess (title, content) -> do
            now <- liftIO getCurrentTime
            nid <- runDB $ insert $ News now title content
            setMessage "News item posted"
            redirect RedirectTemporary $ NewsItemR nid
        _ -> return ()
    defaultLayout $ do
        setTitle "Add news item"
        [hamlet|\
<form method="post" action="@{NewsR}">
    <table>
        \^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Post">
|]

getNewsItemR :: NewsId -> Handler RepHtml
getNewsItemR nid = do
    cacheSeconds 3600
    n <- runDB $ get404 nid
    defaultLayout $ do
        setTitle $ toHtml $ newsTitle n
        addCassius $(cassiusFile "news")
        $(hamletFile "news-item")

getNewsFeedR :: Handler RepAtomRss
getNewsFeedR = do
    cacheSeconds 7200
    news@(newest:_) <- runDB $ selectList [] [NewsWhenDesc] 10 0
    newsFeed Feed
        { feedTitle = "Haskellers News"
        , feedLinkSelf = NewsFeedR
        , feedLinkHome = RootR
        , feedUpdated = newsWhen $ snd newest
        , feedEntries = map go news
        , feedDescription = "Haskellers news feed"
        , feedLanguage = "en"
        }
  where
    go (nid, n) = FeedEntry
        { feedEntryLink = NewsItemR nid
        , feedEntryUpdated = newsWhen n
        , feedEntryTitle = newsTitle n
        , feedEntryContent = newsContent n
        }
