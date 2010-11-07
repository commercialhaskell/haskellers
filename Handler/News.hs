{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.News
    ( getNewsR
    , postNewsR
    , getNewsItemR
    , getNewsFeedR
    ) where

import Haskellers
import Yesod.Helpers.AtomFeed
import Control.Applicative
import Yesod.Form.Nic
import Handler.Admin (requireAdmin)
import Data.Time (getCurrentTime)

#define debugRunDB debugRunDBInner __FILE__ __LINE__

newsForm :: Form s Haskellers (String, Html)
newsForm = fieldsToTable $ (,)
    <$> stringField "Title" Nothing
    <*> nicHtmlField "Content"
        { ffsId = Just "content"
        } Nothing

getNewsR :: Handler RepHtml
getNewsR = do
    mu <- maybeAuth
    cacheSeconds 3600
    news <- debugRunDB $ selectList [] [NewsWhenDesc] 0 0
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
        [$hamlet|
%form!method=post!action=@NewsR@
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit!value=Post
|]

getNewsItemR :: NewsId -> Handler RepHtml
getNewsItemR nid = do
    cacheSeconds 3600
    n <- runDB $ get404 nid
    defaultLayout $ do
        setTitle $ string $ newsTitle n
        $(hamletFile "news-item")

getNewsFeedR :: Handler RepAtom
getNewsFeedR = do
    cacheSeconds 7200
    news@(newest:_) <- runDB $ selectList [] [NewsWhenDesc] 10 0
    atomFeed AtomFeed
        { atomTitle = "Haskellers News"
        , atomLinkSelf = NewsFeedR
        , atomLinkHome = RootR
        , atomUpdated = newsWhen $ snd newest
        , atomEntries = map go news
        }
  where
    go (nid, n) = AtomFeedEntry
        { atomEntryLink = NewsItemR nid
        , atomEntryUpdated = newsWhen n
        , atomEntryTitle = newsTitle n
        , atomEntryContent = newsContent n
        }
