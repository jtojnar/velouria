module Model.NodeType where

import Database.Persist.TH
import Prelude

data NodeType = Page | Topic | Event
    deriving (Show, Read, Eq)
derivePersistField "NodeType"
