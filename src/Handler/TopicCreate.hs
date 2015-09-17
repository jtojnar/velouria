module Handler.TopicCreate where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown
import Model.NodeType
import View.Form

getTopicCreateR, postTopicCreateR :: Slug -> Handler Html
postTopicCreateR = getTopicCreateR
getTopicCreateR slug = do
    Entity forumId _ <- runDB $ getBy404 $ UniqueForumSlug slug
    ((res, widget), enctype) <- runFormPost $ topicCreationForm forumId
    case res of
        FormSuccess topic -> do
            db <- runDB $ insert topic
            print db
            redirect $ TopicViewR slug (nodeSlug topic)
        _ -> defaultLayout $ do
            setTitleI MsgForumCreationTitle
            mkFormView (TopicCreateR slug) (pure widget) enctype

topicCreationForm :: ForumId -> Form Node
topicCreationForm forumId = renderForm $ Node
                                <$> areq textField (bfs MsgTopicTitle) Nothing
                                <*> areq slugField (bfs MsgTopicSlug) Nothing
                                <*> areq markdownField (extendFieldSettings "rows" "15" (bfs MsgTopicBody)) Nothing
                                <*> lift requireAuthId
                                <*> lift (liftIO getCurrentTime)
                                <*> pure forumId
                                <*> pure Topic
                                <*> pure True
                                <*  bootstrapSubmit (BootstrapSubmit MsgCreateTopic "btn-primary btn-lg" [])
