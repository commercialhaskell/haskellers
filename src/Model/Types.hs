module Model.Types where

import Prelude
import Yesod
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
