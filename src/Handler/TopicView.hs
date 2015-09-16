module Handler.TopicView where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown
import View.Form
import Partials (gravatarUrl, fullTimeWidget)

getTopicViewR :: Slug -> Slug -> Handler Html
getTopicViewR forumSlug topicSlug = do
    muser <- maybeAuth
    (parentForumTitle, (E.Value topicId, E.Value topicTitle, E.Value topicBody, E.Value topicCreated, E.Value topicAuthor, E.Value topicAuthorEmail), comments) <- runDB $ do
        Entity forumId forum <- getBy404 $ UniqueForumSlug forumSlug
        topic@(E.Value topicId, _, _, _, _, _) <- eGet404 $ E.from $ \(node `E.InnerJoin` user) -> do
            E.on $ node ^. NodeAuthor E.==. user ^. UserId
            E.where_ (node ^. NodeForum E.==. (E.val forumId) E.&&. node ^. NodeSlug E.==. (E.val topicSlug))
            return (
                node ^. NodeId,
                node ^. NodeTitle,
                node ^. NodeBody,
                node ^. NodeCreated,
                user ^. UserNickname,
                user ^. UserEmail
              )
        comments <- E.select $ E.from $ \(comment `E.InnerJoin` user) -> do
            E.on $ comment ^. CommentAuthor E.==. user ^. UserId
            E.where_ (comment ^. CommentNode E.==. (E.val topicId))
            E.orderBy [E.asc (comment ^. CommentCreated)]
            return (
                comment ^. CommentId,
                comment ^. CommentBody,
                comment ^. CommentCreated,
                user ^. UserNickname,
                user ^. UserEmail
              )
        return (forumTitle forum, topic, comments)
    (widget, enctype) <- (case muser of
            Just _ -> generateFormPost $ replyForm topicId
            _ -> return (mempty, mempty))
    defaultLayout $ do
        setTitle $ toHtml topicTitle
        let replyFormWidget = mkFormView (TopicReplyR forumSlug topicSlug) (pure widget) enctype
        $(widgetFile "topic")

getTopicReplyR, postTopicReplyR :: Slug -> Slug -> Handler Html
getTopicReplyR = postTopicReplyR
postTopicReplyR forumSlug topicSlug = do
    Entity forumId _ <- runDB $ getBy404 $ UniqueForumSlug forumSlug
    Entity topicId topic <- runDB $ getBy404 $ UniqueNodeSlug forumId topicSlug
    ((res, widget), enctype) <- runFormPost $ replyForm topicId
    case res of
        FormSuccess comment -> do
            commentId <- runDB $ insert comment
            print $ show commentId
            redirect $ TopicViewR forumSlug topicSlug
        _ -> defaultLayout $ do
            setTitleI $ MsgReplyTitle $ nodeTitle topic
            mkFormView (TopicReplyR forumSlug topicSlug) (pure widget) enctype

replyForm :: NodeId -> Form Comment
replyForm topicId = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 4) (ColMd 0) (ColMd 6)) $ Comment
                                <$> pure topicId
                                <*> lift requireAuthId
                                <*> areq markdownField (extendFieldSettings "rows" "10" (bfs MsgReplyBody)) Nothing
                                <*> lift (liftIO getCurrentTime)
                                <*  bootstrapSubmit (BootstrapSubmit MsgReplyButton "btn-primary btn-lg" [])
