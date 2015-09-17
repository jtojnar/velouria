module Handler.ForumView where

import Import
import Partials
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))

getForumViewR :: Slug -> Handler Html
getForumViewR slug = do
    muser <- maybeAuth
    Entity forumId currentForum <- runDB $ getBy404 $ UniqueForumSlug slug
    (fora, topics) <- runDB $ do
        topics <- selectTopics forumId
        fora <- selectFora forumId
        return (fora, topics)
    defaultLayout $ do
        setTitle $ toHtml $ forumTitle currentForum
        $(widgetFile "forum")

type TopicListInfo = (E.Value Text, E.Value Slug, E.Value Text, E.Value UTCTime, E.Value Int, (E.Value (Maybe Text)), E.Value (Maybe UTCTime))

topicListPartial :: Forum -> [TopicListInfo] -> Widget
topicListPartial forum topics = $(widgetFile "@topicList")

selectFora forumId = selectList [ForumParent ==. Just forumId] [Desc ForumTitle]

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
