{-# LANGUAGE OverloadedStrings #-}
module SESCreds where

import Data.ByteString.Char8 (ByteString)

access, secret :: ByteString
access = "fake-access-key"
secret = "fake-secret-key"
