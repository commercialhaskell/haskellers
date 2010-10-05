{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    ) where

import Haskellers
import Control.Applicative

userForm :: User -> Form s m User
userForm u = fieldsToTable $ User
    <$> stringField "Full name"
            { ffsId = Just "full-name"
            } (Just $ userFullName u)
    <*> maybeStringField "Website"
            { ffsId = Just "website"
            } (Just $ userWebsite u)
    <*> maybeEmailField "Email"
            { ffsTooltip = [$hamlet|
Your email is protected via $
%a!href="http://www.google.com/recaptcha" recaptcha
\ and used for $
%a!href="http://gravatar.com/" gravatar
\ profile images.
|]
            , ffsId = Just "email"
            } (Just $ userEmail u)
    <*> maybeTextareaField "Description"
            { ffsId = Just "desc"
            } (Just $ userDesc u)
    <*> pure (userHuman u)
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
    defaultLayout $ do
        addStyle $(cassiusFile "profile")
        $(hamletFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR
