{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Bling
    ( getBlingR
    ) where

import Foundation
import StaticFiles (bling_monads_in_disguise_png)

getBlingR :: Handler RepHtml
getBlingR = defaultLayout $ do
    setTitle "Haskell Bling"
    $(widgetFile "bling")
