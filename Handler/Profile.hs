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

import Foundation
import Handler.Root (yearField)
import Control.Applicative
import Handler.Root (gravatar)
import Yesod.Form.Jquery
import StaticFiles (jquery_cookie_js, badge_png)
import Data.Maybe (isJust)
import Control.Monad (filterM, forM_, unless)
import Yesod.Form
import Yesod.Auth (requireAuthId)
import Control.Arrow ((&&&))
import Data.Time
import Data.Text (pack)
import qualified Data.Text as T
import Data.Char (isDigit)
import Yesod.Static

screenNameFormlet :: UserId -> Html -> MForm Haskellers Haskellers (FormResult ScreenName, Widget)
screenNameFormlet uid = renderTable $ ScreenName
    <$> pure uid
    <*> areq (selectFieldList servopts) "Service" Nothing
    <*> areq textField "Screen name" Nothing
  where
    servopts = map (T.pack . show &&& id) [minBound..maxBound]

userForm :: Int -> User -> Html -> MForm Haskellers Haskellers (FormResult User, Widget)
userForm maxY u = renderTable $ User
    <$> areq textField "Full name"
            { fsId = Just "full-name"
            } (Just $ userFullName u)
    <*> aopt urlField "Website"
            { fsId = Just "website"
            } (Just $ userWebsite u)
    <*> pure (userEmail u)
    <*> pure (userVerifiedEmail u)
    <*> pure (userVerkey u)
    <*> aopt (yearField 1985 maxY) "Using Haskell since"
            { fsTooltip = Just "Don't worry if you took breaks from Haskell, just put the year you wrote your first Haskell code"
            } (Just $ userHaskellSince u)
    <*> aopt textareaField "Description"
            { fsId = Just "desc"
            } (Just $ userDesc u)
    <*> areq boolField "Visible?"
            { fsTooltip = Just "Do you want your profile to be displayed on the homepage?"
            } (Just $ userVisible u)
    <*> pure (userReal u)
    <*> pure (userRealPic u)
    <*> pure (userAdmin u)
    <*> aopt (selectFieldList empOpts) "Employment status"
            { fsTooltip = Just "Just remember, this information will be public, meaning your current employer will be able to see it!"
            } (Just $ userEmployment u)
    <*> pure (userBlocked u)
    <*> areq boolField "Public email address?"
            { fsTooltip = Just "Selecting this will allow anyone to see your email address, without passing a captcha. May lead to spam."
            } (Just $ userEmailPublic u)
    <*> aopt textField "Location"
            { fsTooltip = Just "This should be a human-readable string"
            , fsId = Just "location"
            } (Just $ userLocation u)
    <*> aopt doubleField "Longitude"
            { fsId = Just "longitude"
            } (Just $ userLongitude u)
    <*> aopt doubleField "Latitude"
            { fsId = Just "latitude"
            } (Just $ userLatitude u)
    <*> aopt (checkBool validGooglePlus ("Not a Google+ link" :: T.Text) urlField)
            "Google+ Profile Link" (Just $ userGooglePlus u)
  where
    empOpts = map (pack . prettyEmployment &&& id) [minBound..maxBound]
    validGooglePlus t = "https://plus.google.com/" `T.isPrefixOf` t

getProfileR :: Handler RepHtml
getProfileR = do
    Entity uid u <- requireAuth
    now <- liftIO getCurrentTime
    let (maxY, _, _) = toGregorian $ utctDay now
    ((res, form), enctype) <- runFormPostNoNonce $ userForm (fromInteger maxY) u
    musername <- fmap (fmap entityVal) $ runDB $ getBy $ UniqueUsernameUser uid
    case res of
        FormSuccess u' -> do
            runDB $ replace uid u'
            setMessage "Updated your profile"
            redirect ProfileR
        _ -> return ()
    y <- getYesod
    skills <- runDB $ selectList [] [Asc SkillName] >>= mapM (\(Entity sid s) -> do
        x <- getBy $ UniqueUserSkill uid sid
        return $ ((sid, s), isJust x)
        )
    packages <- runDB $ selectList [PackageUser ==. uid] [Asc PackageName]
    idents <- runDB $ selectList [IdentUser ==. uid] [Asc IdentIdent]
    screenNames <- runDB $ selectList [ScreenNameUser ==. uid]
                    [Asc ScreenNameService, Asc ScreenNameName]
    ((_, screenNameForm), _) <- runFormGet $ screenNameFormlet uid
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
    isWeb t = "http://" `T.isPrefixOf` t || "https://" `T.isPrefixOf` t

postProfileR :: Handler RepHtml
postProfileR = getProfileR

postDeleteAccountR :: Handler ()
postDeleteAccountR = do
    uid <- requireAuthId
    runDB $ do
        updateWhere [TopicCreator ==. Just uid] [TopicCreator =. Nothing]
        updateWhere [TopicMessageCreator ==. Just uid]
                    [TopicMessageCreator =. Nothing]
        deleteWhere [IdentUser ==. uid]
        deleteWhere [UserSkillUser ==. uid]
        deleteWhere [PackageUser ==. uid]
        {- FIXME
        updateWhere [MessageFrom Nothing] [MessageFromEq $ Just uid]
        updateWhere [MessageRegarding Nothing] [MessageRegardingEq $ Just uid]
        -}
        delete uid
    setMessage "Your account has been deleted."
    redirect RootR

postSkillsR :: Handler ()
postSkillsR = do
    uid <- requireAuthId
    allSkills <- fmap (map entityKey) $ runDB $ selectList [] []
    let toBool = maybe False (const True)
    skills <- flip filterM allSkills $ \sid ->
        fmap toBool $ runInputPost (iopt textField $ toPathPiece sid)
    runDB $ do
        deleteWhere [UserSkillUser ==. uid]
        forM_ skills $ \sid -> insert (UserSkill uid sid)
    setMessage "Your skills have been updated"
    redirect ProfileR

postDeleteIdentR :: IdentId -> Handler ()
postDeleteIdentR iid = do
    uid <- requireAuthId
    i <- runDB $ get404 iid
    unless (uid == identUser i) notFound
    idents <- runDB $ count [IdentUser ==. uid]
    if idents > 1
        then do
            runDB $ delete iid
            setMessage "Identifier deleted"
        else setMessage "You cannot delete your last identifier"
    redirect ProfileR

badge_png_plain :: Route Static
badge_png_plain = StaticRoute ["badge.png"] []

postRequestRealR :: Handler ()
postRequestRealR = do
    Entity uid u <- requireAuth
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
    redirect ProfileR

hasGoodName :: String -> Bool
hasGoodName "" = False
hasGoodName ('h':'t':'t':'p':_) = False
hasGoodName _ = True

postRequestRealPicR :: Handler ()
postRequestRealPicR = do
    Entity uid u <- requireAuth
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
    redirect ProfileR

postRequestUnblockR :: Handler ()
postRequestUnblockR = do
    Entity uid u <- requireAuth
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
    redirect ProfileR

postRequestSkillR :: Handler ()
postRequestSkillR = do
    uid <- requireAuthId
    res <- runInputPost $ iopt textField "skill"
    case res of
        Just skill -> do
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
        Nothing -> setMessage "Invalid skill entered."
    redirect ProfileR

postClearUsernameR :: Handler ()
postClearUsernameR = do
    uid <- requireAuthId
    runDB $ deleteBy $ UniqueUsernameUser uid
    setMessage "Your username has been cleared."
    redirect ProfileR

postSetUsernameR :: Handler ()
postSetUsernameR = do
    uid <- requireAuthId
    res <- runInputPost $ iopt textField "username"
    let musername =
            case res of
                Just x ->
                    if T.all validChar x && not (T.all isDigit x)
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
    redirect ProfileR
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
    uid <- requireAuthId
    ((res, _), _) <- runFormPostNoNonce $ screenNameFormlet uid
    case res of
        FormSuccess sn -> do
            _ <- runDB $ insert sn
            setMessage "Screen name added"
        _ -> setMessage "Invalid screen name"
    redirect ProfileR

postDeleteScreenNameR :: ScreenNameId -> Handler ()
postDeleteScreenNameR snid = do
    uid <- requireAuthId
    sn <- runDB $ get404 snid
    unless (screenNameUser sn == uid) notFound
    runDB $ delete snid
    setMessage "Screen name deleted"
    redirect ProfileR
