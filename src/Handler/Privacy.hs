module Handler.Privacy (getPrivacyR) where

import Import

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
  setTitle "Privacy policy"
  [whamlet|
      <p>Haskellers will only collect the following information about you:

      <ul>
          <li>Information you explicitly enter into the profile page
          <li>Email addresses or similar identifiers provided by third party identity providers (e.g. Google, Facebook, or OpenID)

      <p>You can delete your account at any time from your profile page.
  |]
