{-# LANGUAGE QuasiQuotes #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    ) where

import Haskellers

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    (res, form, enctype) <- runFormPost $ toForm $ Just u
    case res of
        FormSuccess u' -> runDB $ replace uid u'
        _ -> return ()
    defaultLayout $ do
        addStyle [$cassius|
#full-name, #website, #email
    width: 300px
#desc
    width: 300px
    height: 70px
|]
        [$hamlet|
%form!method=post!action=@ProfileR@!enctype=$enctype$
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit!value="Update Profile"
|]

postProfileR :: Handler RepHtml
postProfileR = getProfileR
