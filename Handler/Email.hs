{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Email
    ( postResetEmailR
    , postSendVerifyR
    , getVerifyEmailR
    ) where

import Haskellers
import Control.Monad (when)
import Yesod.Mail
import System.Random (newStdGen)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.UTF8 as LU

postResetEmailR :: Handler ()
postResetEmailR = do
    (uid, _) <- requireAuth
    runDB $ update uid
        [ UserVerifiedEmail False
        , UserEmail Nothing
        , UserVerkey Nothing
        ]
    setMessage "Email address reset. Please verify a new address."
    redirect RedirectTemporary ProfileR

getVerifyEmailR :: String -> Handler ()
getVerifyEmailR verkey = do
    (uid, u) <- requireAuth
    if Just verkey == userVerkey u && isJust (userEmail u)
        then do
            runDB $ update uid
                [ UserVerifiedEmail True
                , UserVerkey Nothing
                ]
            setMessage "Your email address has been verified."
        else setMessage "Invalid verification key"
    redirect RedirectTemporary ProfileR

postSendVerifyR :: Handler ()
postSendVerifyR = do
    (uid, u) <- requireAuth
    when (userVerifiedEmail u) $ do
        setMessage "You already have a verified email address."
        redirect RedirectTemporary ProfileR
    (res, _, _) <- runFormPost $ emailInput "email"
    case res of
        FormSuccess email -> do
            stdgen <- liftIO newStdGen
            let verkey = fst $ randomString 10 stdgen
            runDB $ update uid [ UserEmail $ Just email
                               , UserVerkey $ Just verkey
                               ]
            render <- getUrlRender
            let url = render $ VerifyEmailR verkey
            liftIO $ sendmail $ LU.fromString $ unlines
                [ "From: noreply@haskellers.com"
                , "To: " ++ email
                , "Subject: Verify your email address"
                , ""
                , "Please go to the URL below to verify your email address"
                , ""
                , url
                ]
            setMessage "A confirmation link has been sent."
        _ -> setMessage "You entered an invalid email address."
    redirect RedirectTemporary ProfileR
