{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Bling
    ( getBlingR
    ) where

import Import

getBlingR :: Handler Html
getBlingR = defaultLayout $ do
    setTitle "Haskell Bling"
    $(widgetFile "bling")
