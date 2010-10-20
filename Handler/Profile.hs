{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteAccountR
    , postSkillsR
    , postDeleteIdentR
    , postRequestRealR
    , postRequestRealPicR
    , postRequestUnblockR
    , postRequestSkillR
    , postSetUsernameR
    , postClearUsernameR
    , postScreenNamesR
    , postDeleteScreenNameR
    ) where

#define debugRunDB debugRunDBInner __FILE__ __LINE__

import Haskellers
import Control.Applicative
import Handler.Root (gravatar)
import Yesod.Form.Jquery
import StaticFiles (jquery_cookie_js, badge_png)
import Data.Maybe (isJust)
import Control.Monad (filterM, forM_, unless)
import Yesod.Form.Core
import Yesod.Form.Profiles
import Control.Arrow ((&&&))
import Data.Time (getCurrentTime)

screenNameFormlet :: UserId -> Form s y ScreenName
screenNameFormlet uid = fieldsToTable $ ScreenName
    <$> pure uid
    <*> selectField servopts "Service" Nothing
    <*> stringField "Screen name" Nothing
  where
    servopts = map (id &&& show) [minBound..maxBound]

userForm :: User -> Form s m User
userForm u = fieldsToTable $ User
    <$> stringField "Full name"
            { ffsId = Just "full-name"
            } (Just $ userFullName u)
    <*> maybeUrlField "Website"
            { ffsId = Just "website"
            } (Just $ userWebsite u)
    <*> pure (userEmail u)
    <*> pure (userVerifiedEmail u)
    <*> pure (userVerkey u)
    <*> maybeHaskellSinceField "Using Haskell since"
            { ffsTooltip = "Don't worry if you took breaks from Haskell, just put the year you wrote your first Haskell code"
            } (Just $ userHaskellSince u)
    <*> maybeTextareaField "Description"
            { ffsId = Just "desc"
            } (Just $ userDesc u)
    <*> boolField "Visible?"
            { ffsTooltip = "Do you want your profile to be displayed on the homepage?"
            } (Just $ userVisible u)
    <*> pure (userReal u)
    <*> pure (userRealPic u)
    <*> pure (userAdmin u)
    <*> maybeSelectField empOpts "Employment status"
            { ffsTooltip = "Just remember, this information will be public, meaning your current employer will be able to see it!"
            } (Just $ userEmployment u)
    <*> pure (userBlocked u)
    <*> boolField "Public email address?"
            { ffsTooltip = "Selecting this will allow anyone to see your email address, without passing a captcha. May lead to spam."
            } (Just $ userEmailPublic u)
    <*> maybeStringField "Location"
            { ffsTooltip = "This should be a human-readable string"
            , ffsId = Just "location"
            } (Just $ userLocation u)
    <*> maybeDoubleField "Longitude"
            { ffsId = Just "longitude"
            } (Just $ userLongitude u)
    <*> maybeDoubleField "Latitude"
            { ffsId = Just "latitude"
            } (Just $ userLatitude u)
  where
    empOpts = map (id &&& prettyEmployment) [minBound..maxBound]
    maybeHaskellSinceField = optionalFieldHelper haskellSinceFieldProfile
    haskellSinceFieldProfile = intFieldProfile
        { fpParse =
            \s ->
                case fpParse intFieldProfile s of
                    Left e -> Left e
                    Right i -> validSinceYear i
        }
    validSinceYear y
        | y >= 1985 && y <= 2010 = Right y
        | otherwise = Left "Unless you've got a time machine, I don't think that's possible"

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    (res, form, enctype) <- runFormPost $ userForm u
    musername <- fmap (fmap snd) $ debugRunDB $ getBy $ UniqueUsernameUser uid
    case res of
        FormSuccess u' -> do
            debugRunDB $ replace uid u'
            setMessage "Updated your profile"
            redirect RedirectTemporary ProfileR
        _ -> return ()
    y <- getYesod
    skills <- debugRunDB $ selectList [] [SkillNameAsc] 0 0 >>= mapM (\(sid, s) -> do
        x <- getBy $ UniqueUserSkill uid sid
        return $ ((sid, s), isJust x)
        )
    packages <- debugRunDB $ selectList [PackageUserEq uid] [PackageNameAsc] 0 0
    idents <- debugRunDB $ selectList [IdentUserEq uid] [IdentIdentAsc] 0 0
    screenNames <- debugRunDB $ selectList [ScreenNameUserEq uid]
                    [ScreenNameServiceAsc, ScreenNameNameAsc] 0 0
    (_, screenNameForm, _) <- runFormGet $ screenNameFormlet uid
    defaultLayout $ do
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        setTitle "Edit Your Profile"
        addStyle $(cassiusFile "profile")
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addJavascript $(juliusFile "profile")
        $(hamletFile "profile")
  where
    notOne [_] = False
    notOne _ = True

postProfileR :: Handler RepHtml
postProfileR = getProfileR

postDeleteAccountR :: Handler ()
postDeleteAccountR = do
    (uid, _) <- requireAuth
    debugRunDB $ do
        deleteWhere [IdentUserEq uid]
        deleteWhere [UserSkillUserEq uid]
        delete uid
    setMessage "Your account has been deleted."
    redirect RedirectTemporary RootR

postSkillsR :: Handler ()
postSkillsR = do
    (uid, _) <- requireAuth
    allSkills <- fmap (map fst) $ debugRunDB $ selectList [] [] 0 0
    skills <- flip filterM allSkills $ \sid ->
        runFormPost' (boolInput $ showIntegral sid)
    debugRunDB $ do
        deleteWhere [UserSkillUserEq uid]
        forM_ skills $ \sid -> insert (UserSkill uid sid)
    setMessage "Your skills have been updated"
    redirect RedirectTemporary ProfileR

postDeleteIdentR :: IdentId -> Handler ()
postDeleteIdentR iid = do
    (uid, _) <- requireAuth
    i <- debugRunDB $ get404 iid
    unless (uid == identUser i) notFound
    idents <- debugRunDB $ count [IdentUserEq uid]
    if idents > 1
        then do
            debugRunDB $ delete iid
            setMessage "Identifier deleted"
        else setMessage "You cannot delete your last identifier"
    redirect RedirectTemporary ProfileR

badge_png_plain :: StaticRoute
badge_png_plain = StaticRoute ["badge.png"] []

postRequestRealR :: Handler ()
postRequestRealR = do
    (uid, u) <- requireAuth
    if userReal u
        then setMessage "You already have verified user status"
        else if userVerifiedEmail u && hasGoodName (userFullName u)
            then do
                now <- liftIO getCurrentTime
                _ <- debugRunDB $ insert $ Message
                    { messageClosed = False
                    , messageWhen = now
                    , messageFrom = Just uid
                    , messageRegarding = Just uid
                    , messageText = Textarea "Requesting verified user status"
                    }
                setMessage "Your request has been logged. Good luck!"
            else setMessage "Before requesting verified user status, please enter your name and verify your email address."
    redirect RedirectTemporary ProfileR

hasGoodName :: String -> Bool
hasGoodName "" = False
hasGoodName ('h':'t':'t':'p':_) = False
hasGoodName _ = True

postRequestRealPicR :: Handler ()
postRequestRealPicR = do
    (uid, u) <- requireAuth
    if userRealPic u
        then setMessage "You already have real picture status"
        else if userVerifiedEmail u && hasGoodName (userFullName u)
            then do
                now <- liftIO getCurrentTime
                _ <- debugRunDB $ insert $ Message
                    { messageClosed = False
                    , messageWhen = now
                    , messageFrom = Just uid
                    , messageRegarding = Just uid
                    , messageText = Textarea "Requesting real picture status"
                    }
                setMessage "Your request has been logged. Good luck!"
            else setMessage "Before requesting real picture status, please enter your name and verify your email address."
    redirect RedirectTemporary ProfileR

postRequestUnblockR :: Handler ()
postRequestUnblockR = do
    (uid, u) <- requireAuth
    if userBlocked u
        then do
            now <- liftIO getCurrentTime
            _ <- debugRunDB $ insert $ Message
                { messageClosed = False
                , messageWhen = now
                , messageFrom = Just uid
                , messageRegarding = Just uid
                , messageText = Textarea "Requesting unblock"
                }
            setMessage "Your request has been logged. Good luck!"
        else setMessage "Your account isn't blocked."
    redirect RedirectTemporary ProfileR

postRequestSkillR :: Handler ()
postRequestSkillR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPost $ stringInput "skill"
    case res of
        FormSuccess skill -> do
            now <- liftIO getCurrentTime
            _ <- debugRunDB $ insert $ Message
                { messageClosed = False
                , messageWhen = now
                , messageFrom = Just uid
                , messageRegarding = Nothing
                , messageText = Textarea $ unlines
                    [ "Requesting new skill"
                    , ""
                    , skill
                    ]
                }
            setMessage "Your skill request has been logged."
        _ -> setMessage "Invalid skill entered."
    redirect RedirectTemporary ProfileR

postClearUsernameR :: Handler ()
postClearUsernameR = do
    (uid, _) <- requireAuth
    debugRunDB $ deleteBy $ UniqueUsernameUser uid
    setMessage "Your username has been cleared."
    redirect RedirectTemporary ProfileR

postSetUsernameR :: Handler ()
postSetUsernameR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPost $ stringInput "username"
    let musername =
            case res of
                FormSuccess x ->
                    if all validChar x && readIntegral x == (Nothing :: Maybe UserId)
                        then Just x
                        else Nothing
                _ -> Nothing
    case musername of
        Nothing -> setMessage "Invalid username"
        Just un -> do
            x <- debugRunDB $ insertBy $ Username uid un
            case x of
                Left _ -> setMessage "Username already in use"
                Right _ -> setMessage "Your username is set!"
    redirect RedirectTemporary ProfileR
  where
    validChar '-' = True
    validChar '_' = True
    validChar c
        | 'A' <= c && c <= 'Z' = True
        | 'a' <= c && c <= 'z' = True
        | '0' <= c && c <= '9' = True
    validChar _ = False

postScreenNamesR :: Handler ()
postScreenNamesR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPost $ screenNameFormlet uid
    case res of
        FormSuccess sn -> do
            _ <- runDB $ insert sn
            setMessage "Screen name added"
        _ -> setMessage "Invalid screen name"
    redirect RedirectTemporary ProfileR

postDeleteScreenNameR :: ScreenNameId -> Handler ()
postDeleteScreenNameR snid = do
    (uid, _) <- requireAuth
    sn <- runDB $ get404 snid
    unless (screenNameUser sn == uid) notFound
    runDB $ delete snid
    setMessage "Screen name deleted"
    redirect RedirectTemporary ProfileR
