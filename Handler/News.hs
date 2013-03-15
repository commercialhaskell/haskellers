{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.News
    ( getNewsR
    , postNewsR
    , getNewsItemR
    , getNewsFeedR
    ) where

import Import
import Yesod.Feed
import Control.Applicative
import Yesod.Form.Nic
import Handler.Admin (requireAdmin)
import Data.Time (getCurrentTime)
import Data.Text (Text)

newsForm :: Form (Text, Html)
newsForm = renderTable $ (,)
    <$> areq textField "Title" Nothing
    <*> areq nicHtmlField "Content"
        { fsId = Just "content"
        } Nothing

getNewsR :: Handler RepHtml
getNewsR = do
    mu <- maybeAuth
    cacheSeconds 3600
    news <- runDB $ selectList [] [Desc NewsWhen]
    ((_, form), _) <- runFormGet newsForm

    now <- liftIO getCurrentTime
    let fuzzyDiffTime = humanReadableTimeDiff now

    defaultLayout $ do
        setTitle "Haskellers News Archive"
        $(widgetFile "news")
  where
    newsAdmin = $(cassiusFile "templates/news-admin.cassius")

postNewsR :: Handler RepHtml
postNewsR = do
    requireAdmin
    ((res, form), _) <- runFormPostNoToken newsForm
    case res of
        FormSuccess (title, content) -> do
            now <- liftIO getCurrentTime
            nid <- runDB $ insert $ News now title content
            setMessage "News item posted"
            redirect $ NewsItemR nid
        _ -> return ()
    defaultLayout $ do
        setTitle "Add news item"
        [whamlet|\
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
        toWidget $(cassiusFile "templates/news.cassius")
        $(widgetFile "news-item")

getNewsFeedR :: Handler TypedContent
getNewsFeedR = do
    cacheSeconds 7200
    news@(newest:_) <- runDB $ selectList [] [Desc NewsWhen, LimitTo 10]
    newsFeed Feed
        { feedTitle = "Haskellers News"
        , feedLinkSelf = NewsFeedR
        , feedLinkHome = RootR
        , feedUpdated = newsWhen $ entityVal newest
        , feedEntries = map go news
        , feedDescription = "Haskellers news feed"
        , feedLanguage = "en"
        , feedAuthor = "Haskellers News"
        }
  where
    go (Entity nid n) = FeedEntry
        { feedEntryLink = NewsItemR nid
        , feedEntryUpdated = newsWhen n
        , feedEntryTitle = newsTitle n
        , feedEntryContent = newsContent n
        }
