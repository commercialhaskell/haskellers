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
    ) where

import Haskellers
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Applicative
import Yesod.Form.Nic
import Control.Monad (unless)

canAddTeam :: Handler Bool
canAddTeam = do
    ma <- maybeAuth
    case ma of
        Nothing -> return False
        Just (_, u) -> return $ userVerifiedEmail u && userReal u && not (userBlocked u)

teamFormlet mt = fieldsToTable $ Team
    <$> stringField "Name" (fmap teamName mt)
    <*> nicHtmlField "Description"
            { ffsId = Just "team-desc"
            } (fmap teamDesc mt)

getTeamsR :: Handler RepHtml
getTeamsR = do
    cat <- canAddTeam
    (form, enctype, nonce) <- generateForm $ teamFormlet Nothing
    teams' <- runDB $ selectList [] [] 0 0 >>= mapM (\(tid, t) -> do
        users <- count [TeamUserTeamEq tid]
        return ((tid, t), users)
        )
    let teams = reverse $ sortBy (comparing snd) teams'
    defaultLayout $ do
        addCassius $(cassiusFile "teams")
        addWidget $(hamletFile "teams")

postTeamsR :: Handler RepHtml
postTeamsR = do
    (uid, _) <- requireAuth
    cat <- canAddTeam
    unless cat $ permissionDenied "Only unblocked, verified users may create special interest groups"
    (res, form, enctype, nonce) <- runFormPost $ teamFormlet Nothing
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
                Just (_, TeamUser _ _ x) -> return (x == Admin, Just x)
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
    users <- runDB $ selectList [TeamUserTeamEq tid] [] 0 0 >>= mapM (\(_, tu) -> do
        let uid = teamUserUser tu
        u <- get404 uid
        return (teamUserStatus tu, (uid, u))
        )
    let admins = map snd $ filter (\(x, _) -> x == Admin) users
    let amembers = map snd $ filter (\(x, _) -> x == ApprovedMember) users
    let umembers = map snd $ filter (\(x, _) -> x == UnapprovedMember) users
    let notMe x = Just x /= fmap fst ma
    (form, enctype, nonce) <- generateForm $ teamFormlet $ Just t
    defaultLayout $ do
        addCassius $(cassiusFile "teams")
        addCassius $(cassiusFile "team")
        addWidget $(hamletFile "team")

postTeamR :: TeamId -> Handler RepHtml
postTeamR tid = do
    t <- runDB $ get404 tid
    (cet, _) <- canEditTeam tid
    unless cet $ permissionDenied "You are not an administrator of this group"
    (res, form, enctype, nonce) <- runFormPost $ teamFormlet $ Just t
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
            setMessage $ string $ "You have left " ++ teamName t
    redirect RedirectTemporary $ TeamR tid

postWatchTeamR :: TeamId -> Handler ()
postWatchTeamR tid = do
    (uid, _) <- requireAuth
    t <- runDB $ get404 tid
    _ <- runDB $ insertBy $ TeamUser tid uid Watching
    setMessage [$hamlet|You are now watching the <abbr title="Special Interest Group">SIG</abbr> $teamName.t$|]
    redirect RedirectTemporary $ TeamR tid

postJoinTeamR :: TeamId -> Handler ()
postJoinTeamR tid = do
    (uid, _) <- requireAuth
    t <- runDB $ get404 tid
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
            runDB $ insert $ TeamUser tid uid UnapprovedMember
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
            runDB $ update tuid [TeamUserStatus ApprovedMember]
            setMessage "Membership approved"
        _ -> notFound
    redirect RedirectTemporary $ TeamR tid

postTeamAdminR :: TeamId -> UserId -> Handler ()
postTeamAdminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (tuid, TeamUser { teamUserStatus = ApprovedMember }) -> do
            runDB $ update tuid [TeamUserStatus Admin]
            setMessage "User promoted to group admin"
        _ -> notFound
    redirect RedirectTemporary $ TeamR tid

postTeamUnadminR :: TeamId -> UserId -> Handler ()
postTeamUnadminR tid uid = do
    requireGroupAdmin tid
    y <- runDB $ getBy $ UniqueTeamUser tid uid
    case y of
        Just (tuid, TeamUser { teamUserStatus = Admin }) -> do
            runDB $ update tuid [TeamUserStatus ApprovedMember]
            setMessage "User no longer an admin"
        _ -> notFound
    redirect RedirectTemporary $ TeamR tid
