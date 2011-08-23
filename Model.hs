{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Model where

import Yesod
import Database.Persist.Base
import Text.Blaze (ToHtml (..))
import Data.Time (UTCTime, Day)
import Data.Text (Text)
import qualified Data.Text as T

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

instance ToHtml Employment where toHtml = toHtml . prettyEmployment

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 (mkPersist sqlSettings) (mkMigrate "migrateAll") $(persistFile "entities")

userFullName' :: User -> Text
userFullName' u =
    let s = userFullName u
     in if T.length s > 50
            then T.take 40 s `T.append` "..."
            else s
