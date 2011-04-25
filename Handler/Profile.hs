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

import Haskellers
import Handler.Root (yearField)
import Control.Applicative
import Handler.Root (gravatar)
import Yesod.Form.Jquery
import StaticFiles (jquery_cookie_js, badge_png)
import Data.Maybe (isJust)
import Control.Monad (filterM, forM_, unless)
import Yesod.Form.Core
import Control.Arrow ((&&&))
import Data.Time
import Data.Text (pack)
import qualified Data.Text as T

screenNameFormlet :: UserId -> Form s y ScreenName
screenNameFormlet uid = fieldsToTable $ ScreenName
    <$> pure uid
    <*> selectField servopts "Service" Nothing
    <*> (stringField "Screen name" Nothing)
  where
    servopts = map (id &&& T.pack . show) [minBound..maxBound]

userForm :: Int -> User -> Form s m User
userForm maxY u = fieldsToTable $ User
    <$> stringField "Full name"
            { ffsId = Just "full-name"
            } (Just $ userFullName u)
    <*> maybeUrlField "Website"
            { ffsId = Just "website"
            } (Just $ userWebsite u)
    <*> pure (userEmail u)
    <*> pure (userVerifiedEmail u)
    <*> pure (userVerkey u)
    <*> yearField 1985 maxY "Using Haskell since"
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
    empOpts = map (id &&& pack . prettyEmployment) [minBound..maxBound]

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    now <- liftIO getCurrentTime
    let (maxY, _, _) = toGregorian $ utctDay now
    (res, form, enctype) <- runFormPostNoNonce $ userForm (fromInteger maxY) u
    musername <- fmap (fmap snd) $ runDB $ getBy $ UniqueUsernameUser uid
    case res of
        FormSuccess u' -> do
            runDB $ replace uid u'
            setMessage "Updated your profile"
            redirect RedirectTemporary ProfileR
        _ -> return ()
    y <- getYesod
    skills <- runDB $ selectList [] [SkillNameAsc] 0 0 >>= mapM (\(sid, s) -> do
        x <- getBy $ UniqueUserSkill uid sid
        return $ ((sid, s), isJust x)
        )
    packages <- runDB $ selectList [PackageUserEq uid] [PackageNameAsc] 0 0
    idents <- runDB $ selectList [IdentUserEq uid] [IdentIdentAsc] 0 0
    screenNames <- runDB $ selectList [ScreenNameUserEq uid]
                    [ScreenNameServiceAsc, ScreenNameNameAsc] 0 0
    (_, screenNameForm, _) <- runFormGet $ screenNameFormlet uid
    defaultLayout $ do
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        setTitle "Edit Your Profile"
        addCassius $(cassiusFile "profile")
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addJulius $(juliusFile "profile")
        $(hamletFile "profile")
  where
    notOne [_] = False
    notOne _ = True

postProfileR :: Handler RepHtml
postProfileR = getProfileR

postDeleteAccountR :: Handler ()
postDeleteAccountR = do
    (uid, _) <- requireAuth
    runDB $ do
        updateWhere [TopicCreatorEq $ Just uid] [TopicCreator Nothing]
        updateWhere [TopicMessageCreatorEq $ Just uid]
                    [TopicMessageCreator Nothing]
        deleteWhere [IdentUserEq uid]
        deleteWhere [UserSkillUserEq uid]
        delete uid
    setMessage "Your account has been deleted."
    redirect RedirectTemporary RootR

postSkillsR :: Handler ()
postSkillsR = do
    (uid, _) <- requireAuth
    allSkills <- fmap (map fst) $ runDB $ selectList [] [] 0 0
    skills <- flip filterM allSkills $ \sid ->
        runFormPost' (boolInput $ toSinglePiece sid)
    runDB $ do
        deleteWhere [UserSkillUserEq uid]
        forM_ skills $ \sid -> insert (UserSkill uid sid)
    setMessage "Your skills have been updated"
    redirect RedirectTemporary ProfileR

postDeleteIdentR :: IdentId -> Handler ()
postDeleteIdentR iid = do
    (uid, _) <- requireAuth
    i <- runDB $ get404 iid
    unless (uid == identUser i) notFound
    idents <- runDB $ count [IdentUserEq uid]
    if idents > 1
        then do
            runDB $ delete iid
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
        else if userVerifiedEmail u && hasGoodName (T.unpack $ userFullName u)
            then do
                now <- liftIO getCurrentTime
                _ <- runDB $ insert $ Message
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
        else if userVerifiedEmail u && hasGoodName (T.unpack $ userFullName u)
            then do
                now <- liftIO getCurrentTime
                _ <- runDB $ insert $ Message
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
            _ <- runDB $ insert $ Message
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
    (res, _, _) <- runFormPostNoNonce $ stringInput "skill"
    case res of
        FormSuccess skill -> do
            now <- liftIO getCurrentTime
            _ <- runDB $ insert $ Message
                { messageClosed = False
                , messageWhen = now
                , messageFrom = Just uid
                , messageRegarding = Nothing
                , messageText = Textarea $ T.unlines
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
    runDB $ deleteBy $ UniqueUsernameUser uid
    setMessage "Your username has been cleared."
    redirect RedirectTemporary ProfileR

postSetUsernameR :: Handler ()
postSetUsernameR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPostNoNonce $ stringInput "username"
    let musername =
            case res of
                FormSuccess x ->
                    if T.all validChar x && fromSinglePiece x == (Nothing :: Maybe UserId)
                        then Just x
                        else Nothing
                _ -> Nothing
    case musername of
        Nothing -> setMessage "Invalid username"
        Just un -> do
            x <- runDB $ insertBy $ Username uid un
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
    (res, _, _) <- runFormPostNoNonce $ screenNameFormlet uid
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
