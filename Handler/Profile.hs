{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteAccountR
    , postSkillsR
    , postDeleteIdentR
    , postRequestRealR
    , postRequestUnblockR
    ) where

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
    <*> pure (userAdmin u)
    <*> maybeSelectField empOpts "Employment status"
            { ffsTooltip = "Just remember, this information will be public, meaning your current employer will be able to see it!"
            } (Just $ userEmployment u)
    <*> pure (userBlocked u)
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
    runDB $ do
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
        runFormPost' (boolInput $ showIntegral sid)
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
        else if userVerifiedEmail u && hasGoodName (userFullName u)
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
  where
    hasGoodName "" = False
    hasGoodName ('h':'t':'t':'p':_) = False
    hasGoodName _ = True

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
