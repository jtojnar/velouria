module Partials where

import Import
import Network.Gravatar
import Data.Time.Format

forumListPartial :: [Entity Forum] -> Widget
forumListPartial fora = $(widgetFile "@forumList")

gravatarUrl :: Text -> Int -> String
gravatarUrl email size = gravatar def {gSize = Just (Size size)} email

fullTimeWidget :: UTCTime -> Widget
fullTimeWidget time = [whamlet|
<time datetime="#{datetime}">#{humantime}
|] where
    datetime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) time
    humantime = formatTime defaultTimeLocale "%c" time
