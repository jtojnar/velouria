module Model.Slug where

import Database.Persist.Sqlite
import ClassyPrelude.Yesod

newtype Slug = Slug {unSlug :: Text}
    deriving (Show, Read, Eq, PathPiece, PersistField, PersistFieldSql)

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug t
    | null t = throwM $ InvalidSlugException t "Empty slug forbidden"
    | any (not . validChar) t = throwM $ InvalidSlugException t "Contains invalid characters"
    | "-" `isPrefixOf` t = throwM $ InvalidSlugException t "Must not start with a hyphen"
    | otherwise = return $ Slug t
  where
    validChar :: Char -> Bool
    validChar c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        c == '.' ||
        c == '-' ||
        c == '_'

data InvalidSlugException = InvalidSlugException !Text !Text
    deriving (Show, Typeable)
instance Exception InvalidSlugException

slugField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Slug
slugField =
    checkMMap go unSlug textField
  where
    go = return . either (Left . tshow) Right . mkSlug
