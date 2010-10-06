{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteAccountR
    ) where

import Haskellers
import Control.Applicative
import Handler.Root (gravatar)
import Yesod.Form.Jquery
import StaticFiles (jquery_cookie_js)

userForm :: User -> Form s m User
userForm u = fieldsToTable $ User
    <$> stringField "Full name"
            { ffsId = Just "full-name"
            } (Just $ userFullName u)
    <*> maybeStringField "Website"
            { ffsId = Just "website"
            } (Just $ userWebsite u)
    <*> pure (userEmail u)
    <*> pure (userVerifiedEmail u)
    <*> pure (userVerkey u)
    <*> maybeTextareaField "Description"
            { ffsId = Just "desc"
            } (Just $ userDesc u)
    <*> boolField "Visible?"
            { ffsTooltip = "Do you want your profile to be displayed on the homepage?"
            } (Just $ userVisible u)
    <*> pure (userReal u)
    <*> pure (userAdmin u)

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
    defaultLayout $ do
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        setTitle "Edit Your Profile"
        addStyle $(cassiusFile "profile")
        addJavascript $(juliusFile "profile")
        $(hamletFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR

postDeleteAccountR :: Handler ()
postDeleteAccountR = do
    (uid, u) <- requireAuth
    runDB $ do
        deleteWhere [IdentUserEq uid]
        delete uid
    setMessage "Your account has been deleted."
    redirect RedirectTemporary RootR
