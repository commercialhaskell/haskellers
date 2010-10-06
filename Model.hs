{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Model where

import Yesod

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
mkPersist [$persist|
User
    fullName String Asc id=full-name
    website String null id=website
    email String null toFormField=maybeEmailField id=email
    verifiedEmail Bool default=false Eq
    desc Textarea null id=desc
    visible Bool default=true Eq
    real Bool default=false
    admin Bool default=false
Ident
    ident String
    user UserId
    UniqueIdent ident
|]

userFullName' :: User -> String
userFullName' u =
    let s = userFullName u
     in if length s > 50
            then take 40 s ++ "..."
            else s
