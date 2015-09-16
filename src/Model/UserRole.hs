module Model.UserRole where

import Database.Persist.TH
import Prelude

data UserRole = Admin | Moderator | RegularUser
    deriving (Show, Read, Eq)
derivePersistField "UserRole"
