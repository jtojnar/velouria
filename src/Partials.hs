module Partials where

import Import

forumListPartial :: [Entity Forum] -> Widget
forumListPartial fora = $(widgetFile "@forumList")

topicListPartial :: Forum -> [Entity Node] -> Widget
topicListPartial forum topics = $(widgetFile "@topicList")
