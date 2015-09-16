module Handler.TopicView where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown
import View.Form

getTopicViewR :: Slug -> Slug -> Handler Html
getTopicViewR forumSlug topicSlug = do
    muser <- maybeAuth
    (topicId, topic, comments) <- runDB $ do
        Entity forumId _ <- getBy404 $ UniqueForumSlug forumSlug
        Entity topicId topic <- getBy404 $ UniqueNodeSlug forumId topicSlug
        comments <- selectList [CommentNode ==. topicId] [Asc CommentCreated]
        return (topicId, topic, map entityVal comments)
    (widget, enctype) <- (case muser of
            Just _ -> generateFormPost $ replyForm topicId
            _ -> return (mempty, mempty))
    defaultLayout $ do
        setTitle $ toHtml $ nodeTitle topic
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
