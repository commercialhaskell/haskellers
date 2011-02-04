{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Model where

import Yesod
import Database.Persist.Base
import Database.Persist.TH (share2, derivePersistField)
import Database.Persist.GenericSql (mkMigrate)
import Text.Blaze (ToHtml (..))
import Data.Time (UTCTime, Day)

data Employment = FullTime | PartTime | FullPartTime | NotLooking
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Employment"

data Service = Twitter | XMPP | AIM | Freenode
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Service"

data TeamUserStatus = Watching | UnapprovedMember | ApprovedMember | Admin
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "TeamUserStatus"

data TopicType = Discussion | Feature | Bug
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "TopicType"

data TopicStatus = Open | Resolved | Closed
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "TopicStatus"

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
    website String Maybe id=website
    email String Maybe toFormField=maybeEmailField id=email Update
    verifiedEmail Bool default=false Eq Update
    verkey String Maybe Update
    haskellSince Int Maybe Asc
    desc Textarea Maybe id=desc
    visible Bool default=true Eq
    real Bool default=false Update Desc Eq
    realPic Bool default=false Update Eq
    admin Bool default=false Update
    employment Employment Maybe
    blocked Bool Update Eq default=false
    emailPublic Bool default=false
    location String Maybe
    longitude Double Maybe Ne
    latitude Double Maybe Ne
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
    closed Bool Update Eq
    when UTCTime Asc
    from UserId Maybe
    regarding UserId Maybe
    text Textarea
News
    when UTCTime Desc
    title String
    content Html
    deriving
Job
    postedBy UserId
    postedAt UTCTime Desc Gt
    title String
    location String
    fillingBy Day Gt
    fullTime Bool
    partTime Bool
    desc Textarea
ScreenName
    user UserId Eq
    service Service Asc
    name String Asc
Team
    name String Asc
    desc Html
    UniqueTeam name
    deriving
TeamUser
    team TeamId Eq
    user UserId Eq
    status TeamUserStatus Update
    UniqueTeamUser team user
TeamNews
    team TeamId Eq In
    when UTCTime Desc
    title String
    content Html
    url String
    deriving
TeamPackage
    team TeamId Eq
    name String Asc
    hackage Bool
    desc String Maybe
    homepage String Maybe
Topic
    team TeamId Eq
    created UTCTime Desc
    type TopicType
    status TopicStatus Update
    creator UserId Maybe Update Eq
    title String
TopicMessage
    topic TopicId Eq
    created UTCTime Asc
    creator UserId Maybe Update Eq
    content Html
    deriving
|]

userFullName' :: User -> String
userFullName' u =
    let s = userFullName u
     in if length s > 50
            then take 40 s ++ "..."
            else s
