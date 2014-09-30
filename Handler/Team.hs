{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Team
    ( getTeamsR
    , postTeamsR
    , getTeamR
    , postTeamR
    , postLeaveTeamR
    , postWatchTeamR
    , postJoinTeamR
    , postApproveTeamR
    , postTeamAdminR
    , postTeamUnadminR
    , getTeamFeedR
    , getUserFeedR
    , getTeamNewsR
    , postTeamPackagesR
    , postDeleteTeamPackageR
    , loginStatus
    ) where

import Import
import Yesod.Feed
import Data.List (sortBy)
import Data.Ord (comparing)
import Yesod.Form.Nic
import Control.Monad (unless)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Network.HTTP.Types (status301)
import Yesod.Auth

loginStatus :: Maybe (Entity User) -> Widget
loginStatus ma = $(widgetFile "login-status")

canAddTeam :: Maybe (Entity User) -> Handler Bool
canAddTeam ma = do
    case ma of
        Nothing -> return False
        Just (Entity _ u) -> return $ userVerifiedEmail u && userReal u && not (userBlocked u)

teamFormlet :: Maybe Team -> Form Team
teamFormlet mt = renderTable $ Team
    <$> areq textField "Name" (fmap teamName mt)
    <*> areq nicHtmlField "Description"
            { fsId = Just "team-desc"
            } (fmap teamDesc mt)

packageFormlet :: TeamId -> Maybe TeamPackage -> Form TeamPackage
packageFormlet tid mtp = renderTable $ TeamPackage
    <$> pure tid
    <*> areq textField "Name" (fmap teamPackageName mtp)
    <*> areq boolField "Available from Hackage?" (fmap teamPackageHackage mtp)
    <*> aopt textField "Description" (fmap teamPackageDesc mtp)
    <*> aopt urlField "Homepage" (fmap teamPackageHomepage mtp)

getTeamsR :: Handler Html
getTeamsR = do
    ma <- maybeAuth
    cat <- canAddTeam ma
    ((_, form), _enctype) <- runFormPost $ teamFormlet Nothing
    teams' <- runDB $ selectList [] [] >>= mapM (\(Entity tid t) -> do
        users <- count [TeamUserTeam ==. tid]
        return ((tid, t), users)
        )
    let teams = reverse $ sortBy (comparing snd) teams'
    defaultLayout $ do
        toWidget $ loginStatus ma
        $(widgetFile "teams")

postTeamsR :: Handler Html
postTeamsR = do
    u@(Entity uid _) <- requireAuth
    cat <- canAddTeam $ Just u
    unless cat $ permissionDenied "Only unblocked, verified users may create special interest groups"
    ((res, form), _enctype) <- runFormPost $ teamFormlet Nothing
    case res of
        FormSuccess team -> runDB $ do
            tid <- insert team
            _ <- insert $ TeamUser tid uid Admin
            lift $ setMessage "Your new group has been created"
            lift $ redirect $ TeamR tid
        _ -> defaultLayout $ do
            toWidget $(cassiusFile "templates/teams.cassius")
            $(widgetFile "teams-form")

canEditTeam :: TeamId -> Handler (Bool, Maybe TeamUserStatus)
canEditTeam tid = do
    ma <- maybeAuth
    case ma of
        Nothing -> return (False, Nothing)
        Just (Entity uid _) -> do
            x <- runDB $ getBy $ UniqueTeamUser tid uid
            case x of
                Just (Entity _ (TeamUser _ _ y)) -> return (y == Admin, Just y)
                _ -> return (False, Nothing)

getTeamR :: TeamId -> Handler Html
getTeamR tid = do
    t <- runDB $ get404 tid
    ma <- maybeAuth
    (cet, status) <- canEditTeam tid
    let isAdmin = status == Just Admin
    let isApprovedMember = status == Just ApprovedMember
    let isUnapprovedMember = status == Just UnapprovedMember
    let isMember = isApprovedMember || isUnapprovedMember
    let isWatching = status == Just Watching
    users <- runDB $ selectList [TeamUserTeam ==. tid] [] >>= mapM (\(Entity _ tu) -> do
        let uid = teamUserUser tu
        u <- get404 uid
        return (teamUserStatus tu, (Entity uid u))
        )
    let admins = map snd $ filter (\(x, _) -> x == Admin) users
    let amembers = map snd $ filter (\(x, _) -> x == ApprovedMember) users
    let umembers = map snd $ filter (\(x, _) -> x == UnapprovedMember) users
    let notMe x = Just x /= fmap entityKey ma
    packages <- runDB $ selectList [TeamPackageTeam ==. tid] [Asc TeamPackageName]
    ((_, form), _enctype) <- runFormPost $ teamFormlet $ Just t
    ((_, addPackage), _) <- runFormPost $ packageFormlet tid Nothing
    defaultLayout $ do
        toWidget $ loginStatus ma
        toWidget $(cassiusFile "templates/teams.cassius")
        $(widgetFile "team")
        toWidgetHead [hamlet|<link href="@{TeamFeedR tid}" type="application/atom+xml" rel="alternate" title="#{teamName t} Updates">
|]

postTeamR :: TeamId -> Handler Html
postTeamR tid = do
    t <- runDB $ get404 tid
    (cet, _) <- canEditTeam tid
    unless cet $ permissionDenied "You are not an administrator of this group"
    ((res, form), _enctype) <- runFormPost $ teamFormlet $ Just t
    case res of
        FormSuccess t' -> do
            runDB $ replace tid t'
            setMessage "Group information updated"
            redirect $ TeamR tid
        _ -> defaultLayout $ do
            toWidget $(cassiusFile "templates/teams.cassius")
            $(widgetFile "team-form")

postLeaveTeamR :: TeamId -> Handler ()
postLeaveTeamR tid = do
    uid <- requireAuthId
    t <- runDB $ get404 tid
    x <- runDB $ getBy $ UniqueTeamUser tid uid
    case x of
        Nothing -> setMessage "You are not in that group"
        Just (Entity _ TeamUser{teamUserStatus = Admin}) ->
            setMessage "Admins may not leave a group. Have another admin remove your admin rights first."
        Just (Entity tuid _) -> do
            runDB $ delete tuid
            setMessage $ toHtml $ "You have left " `T.append` teamName t
    redirect $ TeamR tid

postWatchTeamR :: TeamId -> Handler ()
postWatchTeamR tid = do
    uid <- requireAuthId
    t <- runDB $ get404 tid
    _ <- runDB $ insertBy $ TeamUser tid uid Watching
    setMessage [shamlet|\You are now watching the <abbr title="Special Interest Group">SIG</abbr> #{teamName t}
|]
    redirect $ TeamR tid

postJoinTeamR :: TeamId -> Handler ()
postJoinTeamR tid = do
    uid <- requireAuthId
    _ <- runDB $ get404 tid
    x <- runDB $ getBy $ UniqueTeamUser tid uid
    toJoin <-
        case x of
            Nothing -> return True
            Just (Entity tuid (TeamUser{teamUserStatus = Watching})) -> do
                runDB $ delete tuid
                return True
            _ -> return False
    if toJoin
        then do
            _ <- runDB $ insert $ TeamUser tid uid UnapprovedMember
            setMessage "You have been added as an unapproved member. A group admin must approve your membership for it to become active."
        else setMessage "You are already a member of this group."
    redirect $ TeamR tid

requireGroupAdmin :: TeamId -> Handler ()
requireGroupAdmin tid = do
    uid' <- requireAuthId
    x <- runDB $ getBy $ UniqueTeamUser tid uid'
    case fmap entityVal x of
        Just (TeamUser { teamUserStatus = Admin }) -> return ()
        _ -> notFound

postApproveTeamR :: TeamId -> UserId -> Handler ()
postApproveTeamR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (Entity tuid (TeamUser { teamUserStatus = UnapprovedMember })) -> do
            runDB $ do
                t <- get404 tid
                u <- get404 uid
                addTeamNews tid (T.concat ["New member of ", teamName t, " group"])
                            [shamlet|\
<p>#{userFullName u} is now a member of the #{teamName t} special interest group.
|] $ TeamR tid
                update tuid [TeamUserStatus =. ApprovedMember]
            setMessage "Membership approved"
        _ -> notFound
    redirect $ TeamR tid

postTeamAdminR :: TeamId -> UserId -> Handler ()
postTeamAdminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (Entity tuid (TeamUser { teamUserStatus = ApprovedMember })) -> do
            runDB $ update tuid [TeamUserStatus =. Admin]
            setMessage "User promoted to group admin"
        _ -> notFound
    redirect $ TeamR tid

postTeamUnadminR :: TeamId -> UserId -> Handler ()
postTeamUnadminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (Entity tuid (TeamUser { teamUserStatus = Admin })) -> do
            runDB $ update tuid [TeamUserStatus =. ApprovedMember]
            setMessage "User no longer an admin"
        _ -> notFound
    redirect $ TeamR tid

getTeamFeedR :: TeamId -> Handler TypedContent
getTeamFeedR tid = runDB $ do
    t <- get404 tid
    news <- selectList [TeamNewsTeam ==. tid] [Desc TeamNewsWhen, LimitTo 20]
    updated <-
        case news of
            [] -> liftIO getCurrentTime
            (Entity _ n):_ -> return $ teamNewsWhen n
    lift $ newsFeed Feed
        { feedTitle = teamName t `T.append` " on Haskellers"
        , feedLinkSelf = TeamFeedR tid
        , feedLinkHome = TeamR tid
        , feedUpdated = updated
        , feedEntries = map toAtomEntry news
        , feedDescription = toHtml $ teamName t `T.append` " on Haskellers"
        , feedLanguage = "en"
        , feedAuthor = "Haskellers.com"
        }

getUserFeedR :: UserId -> Handler TypedContent
getUserFeedR uid = runDB $ do
    _ <- get404 uid
    tids <- fmap (map $ teamUserTeam . entityVal) $ selectList [TeamUserUser ==. uid] []
    news <- selectList [TeamNewsTeam <-. tids] [Desc TeamNewsWhen, LimitTo 20]
    updated <-
        case news of
            [] -> liftIO getCurrentTime
            (Entity _ n):_ -> return $ teamNewsWhen n
    lift $ newsFeed Feed
        { feedTitle = "Your Haskellers News Feed"
        , feedLinkSelf = UserFeedR uid
        , feedLinkHome = UserR $ toPathPiece uid
        , feedUpdated = updated
        , feedEntries = map toAtomEntry news
        , feedDescription = "Personal Haskellers feed"
        , feedLanguage = "en"
        , feedAuthor = "Haskellers.com"
        }

toAtomEntry :: Entity TeamNews -> FeedEntry (Route Haskellers)
toAtomEntry (Entity tnid tn) = FeedEntry
    { feedEntryLink = TeamNewsR tnid
    , feedEntryUpdated = teamNewsWhen tn
    , feedEntryTitle = teamNewsTitle tn
    , feedEntryContent = teamNewsContent tn
    }

getTeamNewsR :: TeamNewsId -> Handler ()
getTeamNewsR tnid = do
    tn <- runDB $ get404 tnid
    redirectWith status301 $ teamNewsUrl tn

postTeamPackagesR :: TeamId -> Handler Html
postTeamPackagesR tid = do
    requireGroupAdmin tid
    t <- runDB $ get404 tid
    ((res, form), _) <- runFormPost $ packageFormlet tid Nothing
    case res of
        FormSuccess tp -> do
            runDB $ do
                _ <- insert tp
                addTeamNews tid (T.concat ["New package for ", teamName t, " group"]) [shamlet|\
            \setMessage "Package added"
<p>A new package, #{teamPackageName tp}, has been added to #{teamName t}.
$maybe d <- teamPackageDesc tp
    <p>#{d}
$maybe u <- teamPackageHomepage tp
    <p>
        <a href="#{u}">Visit the homepage.
|] $ TeamR tid
            redirect $ TeamR tid
        _ -> defaultLayout [whamlet|\
<form method="post" action="@{TeamPackagesR tid}">
    <table>
        \^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Add Package">
|]

postDeleteTeamPackageR :: TeamId -> TeamPackageId -> Handler ()
postDeleteTeamPackageR tid tpid = do
    requireGroupAdmin tid
    tp <- runDB $ get404 tpid
    unless (tid == teamPackageTeam tp) notFound
    runDB $ delete tpid
    setMessage "Package deleted"
    redirect $ TeamR tid
