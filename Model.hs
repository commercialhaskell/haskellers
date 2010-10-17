{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Model where

import Yesod
import Database.Persist.Base
import Database.Persist.TH (share2, derivePersistField)
import Database.Persist.GenericSql (mkMigrate)
import Text.Hamlet (ToHtml (..))
import Data.Time (UTCTime, Day)

data Employment = FullTime | PartTime | FullPartTime | NotLooking
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Employment"

prettyEmployment :: Employment -> String
prettyEmployment FullTime = "You can ask me about full-time employment"
prettyEmployment PartTime = "You can ask me about part-time employment"
prettyEmployment FullPartTime = "You can ask me about full- or part-time employment"
prettyEmployment NotLooking = "I am not currently seeking employment"

instance ToHtml Employment where toHtml = string . prettyEmployment

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    fullName String Asc id=full-name
    website String null id=website
    email String null toFormField=maybeEmailField id=email update
    verifiedEmail Bool default=false Eq update
    verkey String null update
    haskellSince Int null Asc
    desc Textarea null id=desc
    visible Bool default=true Eq
    real Bool default=false update Desc Eq
    realPic Bool default=false update Eq
    admin Bool default=false update
    employment Employment null
    blocked Bool update Eq default=false
    location String null
    longitude Double null Ne
    latitude Double null Ne
Username
    user UserId
    username String
    UniqueUsernameUser user
    UniqueUsername username
Ident
    ident String Asc
    user UserId Eq
    UniqueIdent ident
Skill
    name String Asc
UserSkill
    user UserId Eq
    skill SkillId Eq
    UniqueUserSkill user skill
Package
    user UserId Eq
    name String Asc
    UniquePackage user name
Message
    closed Bool update Eq
    when UTCTime Asc
    from UserId null
    regarding UserId null
    text Textarea
News
    when UTCTime Desc
    title String
    content Html
    deriving Show Eq
Job
    postedBy UserId
    postedAt UTCTime Desc
    title String
    fillingBy Day Gt
    fullTime Bool
    partTime Bool
    location String
    desc Textarea
|]

userFullName' :: User -> String
userFullName' u =
    let s = userFullName u
     in if length s > 50
            then take 40 s ++ "..."
            else s
