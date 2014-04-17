{-# LANGUAGE DeriveDataTypeable #-}
module Model
    ( module Model
    , module Model.Types
    ) where

import Prelude
import Yesod
import Data.Text (Text, append)
import Data.Char (isUpper)
import qualified Data.Text as T
import Data.Time (UTCTime, Day)
import Database.Persist.Quasi
import Text.Blaze.Html (ToMarkup (..))
import Data.Typeable (Typeable)
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
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
