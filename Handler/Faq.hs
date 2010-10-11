{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Faq
    ( getFaqR
    ) where

import Haskellers

data Faq = Faq
    { hash :: String
    , question :: String
    , answer :: Html
    }

faqs =
    [ Faq "purpose" "What's the purpose of this site?" [$hamlet|
%p Haskell has a vibrant, talented community of very capable programmers. This site aims to be the meeting point for these developers. By centralizing, we hope to make it easier for employers to find people to fill positions, and thus give Haskell a lower entrance cost into industry.
|]
    , Faq "just-professionals" "I'm just a Haskell hobbyist. Does that mean this site isn't for me?" [$hamlet|
%p While the main purpose of the site is for professionals and industry, there's no reason hobbyists shouldn't join in as well. The secondary mission of this site is to provide social networking. As the site is still young, it's not clear what features will be implemented, but this site will be a great resource for any Haskell programmer.
|]
    , Faq "openid" "How do I create an account? What's OpenID?" [$hamlet|
%p Instead of creating a brand new username/password on Haskellers, you can simply log in with OpenID. Most people out there already have an OpenID: Google, Yahoo!, AOL, Wordpress and many others provide them. If you have a Google or Yahoo! account, just click on the appropriate logo and you will be asked to log in automatically. We also support Facebook logins.
%p Don't worry, we only use this information to authenticate you. We do not request any personal information from your OpenID provider, nor do we ever see your password. The only information Haskellers gets is what you provide us explicitly.
|]
    , Faq "report" "What does reporting a user do, and when should I use it?" [$hamlet|
%p Reporting a user sends a message to the site administrators that a user has been reported. It will also tell us who did the reporting if you are logged in. This is a simple way for you to let us know that there is something inappropriate on a user page. Examples of inappropriate content are:
%ul
    %li Inappropriate profile picture. Comic images and other drawings are allowed, though you are encouraged to use an actual photo.
    %li Inappropriate language. No profanity of any kind is welcome on this site.
    %li Spam links. Each user is allowed to post one link to a website: this link should be describing them. The link need not directly relate to Haskell, but linking to information on pharmaceuticals is clearly not allowed.
    %li A profile clearly not belonging to a Haskell user. We have a very liberal definition of a Haskell user here: even someone who's never written a line of Haskell code but has read about the language is welcome. But beyond that, you don't really have a place on this site, and are simply adding noise to the signal.
|]
    , Faq "add-feature" "I see that you have not implemented feature XYZ. Are you going to?" [$hamlet|
%p
    This site is still in its infancy, so just because we haven't implemented a feature does not mean we won't. If you have a recommendation, feel free to either email the haskell-cafe mailing list or contact $
    %a!href="http://www.haskellers.com/user/16/" Michael Snoyman
    \ directly.
|]
    ]

getFaqR :: Handler RepHtml
getFaqR = defaultLayout $(hamletFile "faq")
