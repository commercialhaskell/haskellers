{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Bling
    ( getBlingR
    ) where

import Haskellers

getBlingR :: Handler RepHtml
getBlingR = defaultLayout $ do
    setTitle "Haskell Bling"
    $(widgetFile "bling")
