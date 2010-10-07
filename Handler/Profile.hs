{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteAccountR
    , postSkillsR
    , postDeleteIdentR
    ) where

import Haskellers
import Control.Applicative
import Handler.Root (gravatar)
import Yesod.Form.Jquery
import StaticFiles (jquery_cookie_js)
import Data.Maybe (isJust)
import Control.Monad (filterM, forM_, unless)
import Yesod.Form.Core
import Yesod.Form.Profiles

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
  where
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
    skills <- runDB $ selectList [] [SkillOrderAsc] 0 0 >>= mapM (\(sid, s) -> do
        x <- getBy $ UniqueUserSkill uid sid
        return $ ((sid, s), isJust x)
        )
    idents <- runDB $ selectList [IdentUserEq uid] [IdentIdentAsc] 0 0
    defaultLayout $ do
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        setTitle "Edit Your Profile"
        addStyle $(cassiusFile "profile")
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
