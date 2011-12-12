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

import Haskellers
import Yesod.Feed
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Applicative
import Yesod.Form.Nic
import Control.Monad (unless)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Text.Hamlet (shamlet)

loginStatus :: Maybe (UserId, User) -> Widget
loginStatus ma = do
    addCassius $(cassiusFile "login-status")
    addWidget $(hamletFile "login-status")

canAddTeam :: Maybe (UserId, User) -> Handler Bool
canAddTeam ma = do
    case ma of
        Nothing -> return False
        Just (_, u) -> return $ userVerifiedEmail u && userReal u && not (userBlocked u)

teamFormlet :: Maybe Team -> Html -> MForm Haskellers Haskellers (FormResult Team, Widget)
teamFormlet mt = renderTable $ Team
    <$> areq textField "Name" (fmap teamName mt)
    <*> areq nicHtmlField "Description"
            { fsId = Just "team-desc"
            } (fmap teamDesc mt)

packageFormlet :: TeamId -> Maybe TeamPackage -> Html -> MForm Haskellers Haskellers (FormResult TeamPackage, Widget)
packageFormlet tid mtp = renderTable $ TeamPackage
    <$> pure tid
    <*> areq textField "Name" (fmap teamPackageName mtp)
    <*> areq boolField "Available from Hackage?" (fmap teamPackageHackage mtp)
    <*> aopt textField "Description" (fmap teamPackageDesc mtp)
    <*> aopt urlField "Homepage" (fmap teamPackageHomepage mtp)

getTeamsR :: Handler RepHtml
getTeamsR = do
    ma <- maybeAuth
    cat <- canAddTeam ma
    ((_, form), enctype) <- runFormPost $ teamFormlet Nothing
    teams' <- runDB $ selectList [] [] >>= mapM (\(tid, t) -> do
        users <- count [TeamUserTeam ==. tid]
        return ((tid, t), users)
        )
    let teams = reverse $ sortBy (comparing snd) teams'
    defaultLayout $ do
        addWidget $ loginStatus ma
        addCassius $(cassiusFile "teams")
        addWidget $(hamletFile "teams")

postTeamsR :: Handler RepHtml
postTeamsR = do
    u@(uid, _) <- requireAuth
    cat <- canAddTeam $ Just u
    unless cat $ permissionDenied "Only unblocked, verified users may create special interest groups"
    ((res, form), enctype) <- runFormPost $ teamFormlet Nothing
    case res of
        FormSuccess team -> runDB $ do
            tid <- insert team
            _ <- insert $ TeamUser tid uid Admin
            lift $ setMessage "Your new group has been created"
            lift $ redirect RedirectTemporary $ TeamR tid
        _ -> defaultLayout $ do
            addCassius $(cassiusFile "teams")
            addWidget $(hamletFile "teams-form")

canEditTeam :: TeamId -> Handler (Bool, Maybe TeamUserStatus)
canEditTeam tid = do
    ma <- maybeAuth
    case ma of
        Nothing -> return (False, Nothing)
        Just (uid, _) -> do
            x <- runDB $ getBy $ UniqueTeamUser tid uid
            case x of
                Just (_, TeamUser _ _ y) -> return (y == Admin, Just y)
                _ -> return (False, Nothing)

getTeamR :: TeamId -> Handler RepHtml
getTeamR tid = do
    t <- runDB $ get404 tid
    ma <- maybeAuth
    (cet, status) <- canEditTeam tid
    let isAdmin = status == Just Admin
    let isApprovedMember = status == Just ApprovedMember
    let isUnapprovedMember = status == Just UnapprovedMember
    let isMember = isApprovedMember || isUnapprovedMember
    let isWatching = status == Just Watching
    users <- runDB $ selectList [TeamUserTeam ==. tid] [] >>= mapM (\(_, tu) -> do
        let uid = teamUserUser tu
        u <- get404 uid
        return (teamUserStatus tu, (uid, u))
        )
    let admins = map snd $ filter (\(x, _) -> x == Admin) users
    let amembers = map snd $ filter (\(x, _) -> x == ApprovedMember) users
    let umembers = map snd $ filter (\(x, _) -> x == UnapprovedMember) users
    let notMe x = Just x /= fmap fst ma
    packages <- runDB $ selectList [TeamPackageTeam ==. tid] [Asc TeamPackageName]
    ((_, form), enctype) <- runFormPost $ teamFormlet $ Just t
    ((_, addPackage), _) <- runFormPost $ packageFormlet tid Nothing
    defaultLayout $ do
        addWidget $ loginStatus ma
        addCassius $(cassiusFile "teams")
        addCassius $(cassiusFile "team")
        addWidget $(hamletFile "team")
        addHamletHead [hamlet|<link href="@{TeamFeedR tid}" type="application/atom+xml" rel="alternate" title="#{teamName t} Updates">
|]

postTeamR :: TeamId -> Handler RepHtml
postTeamR tid = do
    t <- runDB $ get404 tid
    (cet, _) <- canEditTeam tid
    unless cet $ permissionDenied "You are not an administrator of this group"
    ((res, form), enctype) <- runFormPost $ teamFormlet $ Just t
    case res of
        FormSuccess t' -> do
            runDB $ replace tid t'
            setMessage "Group information updated"
            redirect RedirectTemporary $ TeamR tid
        _ -> defaultLayout $ do
            addCassius $(cassiusFile "teams")
            addWidget $(hamletFile "team-form")

postLeaveTeamR :: TeamId -> Handler ()
postLeaveTeamR tid = do
    (uid, _) <- requireAuth
    t <- runDB $ get404 tid
    x <- runDB $ getBy $ UniqueTeamUser tid uid
    case x of
        Nothing -> setMessage "You are not in that group"
        Just (_, TeamUser{teamUserStatus = Admin}) ->
            setMessage "Admins may not leave a group. Have another admin remove your admin rights first."
        Just (tuid, _) -> do
            runDB $ delete tuid
            setMessage $ toHtml $ "You have left " `T.append` teamName t
    redirect RedirectTemporary $ TeamR tid

postWatchTeamR :: TeamId -> Handler ()
postWatchTeamR tid = do
    (uid, _) <- requireAuth
    t <- runDB $ get404 tid
    _ <- runDB $ insertBy $ TeamUser tid uid Watching
    setMessage [shamlet|\You are now watching the <abbr title="Special Interest Group">SIG</abbr> #{teamName t}
|]
    redirect RedirectTemporary $ TeamR tid

postJoinTeamR :: TeamId -> Handler ()
postJoinTeamR tid = do
    (uid, _) <- requireAuth
    _ <- runDB $ get404 tid
    x <- runDB $ getBy $ UniqueTeamUser tid uid
    toJoin <-
        case x of
            Nothing -> return True
            Just (tuid, TeamUser{teamUserStatus = Watching}) -> do
                runDB $ delete tuid
                return True
            _ -> return False
    if toJoin
        then do
            _ <- runDB $ insert $ TeamUser tid uid UnapprovedMember
            setMessage "You have been added as an unapproved member. A group admin must approve your membership for it to become active."
        else setMessage "You are already a member of this group."
    redirect RedirectTemporary $ TeamR tid

requireGroupAdmin :: TeamId -> Handler ()
requireGroupAdmin tid = do
    (uid', _) <- requireAuth
    x <- runDB $ getBy $ UniqueTeamUser tid uid'
    case x of
        Just (_, TeamUser { teamUserStatus = Admin }) -> return ()
        _ -> notFound

postApproveTeamR :: TeamId -> UserId -> Handler ()
postApproveTeamR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (tuid, TeamUser { teamUserStatus = UnapprovedMember }) -> do
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
    redirect RedirectTemporary $ TeamR tid

postTeamAdminR :: TeamId -> UserId -> Handler ()
postTeamAdminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (tuid, TeamUser { teamUserStatus = ApprovedMember }) -> do
            runDB $ update tuid [TeamUserStatus =. Admin]
            setMessage "User promoted to group admin"
        _ -> notFound
    redirect RedirectTemporary $ TeamR tid

postTeamUnadminR :: TeamId -> UserId -> Handler ()
postTeamUnadminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (tuid, TeamUser { teamUserStatus = Admin }) -> do
            runDB $ update tuid [TeamUserStatus =. ApprovedMember]
            setMessage "User no longer an admin"
        _ -> notFound
    redirect RedirectTemporary $ TeamR tid

getTeamFeedR :: TeamId -> Handler RepAtomRss
getTeamFeedR tid = runDB $ do
    t <- get404 tid
    news <- selectList [TeamNewsTeam ==. tid] [Desc TeamNewsWhen, LimitTo 20]
    updated <-
        case news of
            [] -> liftIO getCurrentTime
            (_, n):_ -> return $ teamNewsWhen n
    lift $ newsFeed Feed
        { feedTitle = teamName t `T.append` " on Haskellers"
        , feedLinkSelf = TeamFeedR tid
        , feedLinkHome = TeamR tid
        , feedUpdated = updated
        , feedEntries = map toAtomEntry news
        , feedDescription = toHtml $ teamName t `T.append` " on Haskellers"
        , feedLanguage = "en"
        }

getUserFeedR :: UserId -> Handler RepAtomRss
getUserFeedR uid = runDB $ do
    _ <- get404 uid
    tids <- fmap (map $ teamUserTeam . snd) $ selectList [TeamUserUser ==. uid] []
    news <- selectList [TeamNewsTeam <-. tids] [Desc TeamNewsWhen, LimitTo 20]
    updated <-
        case news of
            [] -> liftIO getCurrentTime
            (_, n):_ -> return $ teamNewsWhen n
    lift $ newsFeed Feed
        { feedTitle = "Your Haskellers News Feed"
        , feedLinkSelf = UserFeedR uid
        , feedLinkHome = UserR $ toSinglePiece uid
        , feedUpdated = updated
        , feedEntries = map toAtomEntry news
        , feedDescription = "Personal Haskellers feed"
        , feedLanguage = "en"
        }

toAtomEntry :: (TeamNewsId, TeamNews) -> FeedEntry HaskellersRoute
toAtomEntry (tnid, tn) = FeedEntry
    { feedEntryLink = TeamNewsR tnid
    , feedEntryUpdated = teamNewsWhen tn
    , feedEntryTitle = teamNewsTitle tn
    , feedEntryContent = teamNewsContent tn
    }

getTeamNewsR :: TeamNewsId -> Handler ()
getTeamNewsR tnid = do
    tn <- runDB $ get404 tnid
    redirectText RedirectPermanent $ teamNewsUrl tn

postTeamPackagesR :: TeamId -> Handler RepHtml
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
            redirect RedirectTemporary $ TeamR tid
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
    redirect RedirectTemporary $ TeamR tid
