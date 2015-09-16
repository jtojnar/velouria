module Handler.ForumCreate where

import Import
import Yesod.Text.Markdown
import Yesod.Form.Bootstrap3
import View.Form

getForumCreateR, postForumCreateR :: Handler Html
postForumCreateR = getForumCreateR
getForumCreateR = do
    ((res, widget), enctype) <- runFormPost $ forumCreationForm
    case res of
        FormSuccess forum -> do
            db <- runDB $ insert forum
            print db
            redirect $ ForumViewR $ forumSlug forum
        _ -> defaultLayout $ do
            setTitleI MsgForumCreationTitle
            mkFormView ForumCreateR (pure widget) enctype

forumCreationForm :: Form Forum
forumCreationForm = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 4) (ColMd 0) (ColMd 6)) $ Forum
                                <$> aopt (selectField findExistingFora) (bfs MsgParentForum) Nothing
                                <*> areq textField (bfs MsgForumTitle) Nothing
                                <*> areq slugField (bfs MsgForumSlug) Nothing
                                <*> aopt markdownField (bfs MsgForumDescription) Nothing
                                <*  bootstrapSubmit (BootstrapSubmit MsgCreateForum "btn-primary btn-lg" [])

findExistingFora :: HandlerT App IO (OptionList ForumId)
findExistingFora = do
            fora <- runDB $ selectList [] [Asc ForumTitle]
            optionsPairs $ map (\(Entity forumId forum) -> (forumTitle forum, forumId)) fora
