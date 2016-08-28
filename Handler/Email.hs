module Handler.Email
    ( postResetEmailR
    , postSendVerifyR
    , getVerifyEmailR
    ) where

import Import
import Control.Monad (when)
import Network.Mail.Mime
import Network.Mail.Mime.SES
import System.Random (newStdGen)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.UTF8 as LU
import Data.Text (pack, unpack)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Yesod.Auth (requireAuthId)

postResetEmailR :: Handler ()
postResetEmailR = do
    uid <- requireAuthId
    runDB $ update uid
        [ UserVerifiedEmail =. False
        , UserEmail =. Nothing
        , UserVerkey =. Nothing
        ]
    setMessage "Email address reset. Please verify a new address."
    redirect ProfileR

getVerifyEmailR :: Text -> Handler ()
getVerifyEmailR verkey = do
    Entity uid u <- requireAuth
    if Just verkey == userVerkey u && isJust (userEmail u)
        then do
            runDB $ update uid
                [ UserVerifiedEmail =. True
                , UserVerkey =. Nothing
                ]
            setMessage "Your email address has been verified."
        else setMessage "Invalid verification key"
    redirect ProfileR

postSendVerifyR :: Handler ()
postSendVerifyR = do
    Entity uid u <- requireAuth
    when (userVerifiedEmail u) $ do
        setMessage "You already have a verified email address."
        redirect ProfileR
    res <- runInputPost $ iopt emailField "email"
    case res of
        Just email -> do
            stdgen <- liftIO newStdGen
            let verkey = pack $ fst $ randomString 10 stdgen
            runDB $ update uid [ UserEmail =. Just email
                               , UserVerkey =. Just verkey
                               ]
            render <- getUrlRender
            let url = render $ VerifyEmailR verkey
            h <- getYesod
            let ses = appSesCreds h email
            renderSendMailSES (httpManager h) ses Mail
                { mailHeaders =
                    [ ("Subject", "Verify your email address")
                    ]
                , mailFrom = Address Nothing "webmaster@haskellers.com"
                , mailTo = [Address Nothing email]
                , mailCc = []
                , mailBcc = []
                , mailParts = return
                    [ Part "text/plain" None Nothing [] $ LU.fromString $ unlines
                        [ "Please go to the URL below to verify your email address."
                        , ""
                        , unpack url
                        ]
                    , Part "text/html" None Nothing [] $ renderHtml [shamlet|\
<img src="#{render (StaticR logo_png)}" alt="Haskellers">
<p>Please go to the URL below to verify your email address.
<p>
    <a href="#{url}">#{url}
|]
                    ]
                }
            setMessage "A confirmation link has been sent."
        Nothing -> setMessage "You entered an invalid email address."
    redirect ProfileR
