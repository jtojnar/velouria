module Handler.ForumView where

import Import
import Partials

getForumViewR :: Slug -> Handler Html
getForumViewR slug = do
    muser <- maybeAuth
    Entity forumId currentForum <- runDB $ getBy404 $ UniqueForumSlug slug
    fora <- runDB $ selectList [ForumParent ==. Just forumId] [Desc ForumTitle]
    topics <- runDB $ selectList [NodeForum ==. forumId] []
    defaultLayout $ do
        setTitle $ toHtml $ forumTitle currentForum
        $(widgetFile "forum")
