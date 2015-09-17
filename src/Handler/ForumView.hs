module Handler.ForumView where

import Import
import Partials
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))
import Text.Markdown

getForumViewR :: Slug -> Handler Html
getForumViewR slug = do
    muser <- maybeAuth
    Entity forumId currentForum <- runDB $ getBy404 $ UniqueForumSlug slug
    let currentForumTitle = forumTitle currentForum
    (fora, topics) <- runDB $ do
        topics <- selectTopics forumId
        fora <- selectFora $ Just forumId
        return (fora, topics)
    defaultLayout $ do
        setTitle $ toHtml $ currentForumTitle
        $(widgetFile "forum")

type TopicListInfo = (E.Value Text, E.Value Slug, E.Value Text, E.Value UTCTime, E.Value Int, E.Value (Maybe Text), E.Value (Maybe UTCTime))
type ForumListInfo = (E.Value Text, E.Value Slug, E.Value (Maybe Markdown), E.Value Int, E.Value (Maybe Text), E.Value (Maybe Slug), E.Value (Maybe Text), E.Value (Maybe UTCTime))

forumListPartial :: [ForumListInfo] -> Widget
forumListPartial fora = $(widgetFile "@forumList")

topicListPartial :: Forum -> [TopicListInfo] -> Widget
topicListPartial forum topics = $(widgetFile "@topicList")

selectFora :: (MonadIO m0) => Maybe ForumId -> SqlPersistT m0 [ForumListInfo]
selectFora forumId = E.select $ E.from $ \(forum `E.LeftOuterJoin` topic `E.LeftOuterJoin` tUser) -> do
    E.on $ topic ?. NodeAuthor E.==. E.just (tUser ^. UserId)
    E.on $ E.just (forum ^. ForumId) E.==. topic ?. NodeForum
    E.where_ (case forumId of
        Nothing -> E.isNothing $ forum ^. ForumParent
        _ -> forum ^. ForumParent E.==. (E.val forumId)
      )
    E.orderBy [E.asc (forum ^. ForumTitle), E.desc (topic ?. NodeCreated)]
    E.groupBy (forum ^. ForumId)
    return (
        forum ^. ForumTitle, -- forumTitle
        forum ^. ForumSlug, -- forumSlug
        forum ^. ForumDescription, -- mforumDescription
        E.count (topic ?. NodeId), -- topicCount
        topic ?. NodeTitle,-- mlastTopicTitle
        topic ?. NodeSlug,-- mlastTopicSlug
        E.just (tUser ^. UserNickname), -- mlastTopicAuthor
        topic ?. NodeCreated -- mlastTopicTime
      )

selectTopics :: (MonadIO m0) => ForumId -> SqlPersistT m0 [TopicListInfo]
selectTopics forumId = E.select $ E.from $ \(node `E.LeftOuterJoin` comment `E.LeftOuterJoin` nUser `E.LeftOuterJoin` cUser) -> do
    E.on $ comment ?. CommentAuthor E.==. E.just (cUser ^. UserId)
    E.on $ node ^. NodeAuthor E.==. nUser ^. UserId
    E.on $ E.just (node ^. NodeId) E.==. comment ?. CommentNode
    E.where_ (node ^. NodeForum E.==. (E.val forumId))
    E.orderBy [E.desc (comment ?. CommentCreated), E.desc (node ^. NodeCreated)]
    E.groupBy (node ^. NodeId)
    return (
        node ^. NodeTitle, -- topicTitle
        node ^. NodeSlug, -- topicSlug
        nUser ^. UserNickname, -- topicAuthor
        node ^. NodeCreated, -- topicCreated
        E.count (comment ?. CommentId), -- commentCount
        E.just (cUser ^. UserNickname), -- mlastCommentAuthor
        comment ?. CommentCreated -- mlastCommentTime
      )
