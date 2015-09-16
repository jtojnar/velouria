module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import qualified Database.Esqueleto as E
import Model.NodeType
import Model.Slug
import Model.UserRole
import Yesod.Text.Markdown()
import Text.Markdown

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/Model/schema")


isAdmin :: User -> Bool
isAdmin user = userRole user == Admin

eGet404 query = do
    res <- E.select query
    case res of
        [] -> notFound
        x:_ -> return x
