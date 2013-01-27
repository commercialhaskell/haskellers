module Model where

import Prelude
import Yesod
import Data.Text (Text, append)
import Data.Char (isUpper)
import qualified Data.Text as T
import Data.Time (UTCTime, Day)
import Database.Persist.Quasi
import Text.Blaze.Html (ToMarkup (..))

data Employment = FullTime | PartTime | FullPartTime | NotLooking
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Employment"

data Service = Twitter | XMPP | AIM | Freenode | GooglePlus
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

instance ToMarkup Employment where toMarkup = toMarkup . prettyEmployment

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings
        { psToDBName = \t ->
            if not (T.null t) && isUpper (T.head t)
                then "Haskellers__" `append` psToDBName upperCaseSettings t
                else psToDBName upperCaseSettings t
        } "config/models")

userFullName' :: User -> Text
userFullName' u =
    let s = userFullName u
     in if T.length s > 50
            then T.take 40 s `T.append` "..."
            else s
