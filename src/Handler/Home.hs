module Handler.Home where

import Import
import Handler.ForumView (forumListPartial, selectFora)

getHomeR :: Handler Html
getHomeR = do
    master <- getYesod
    muser <- maybeAuth
    fora <- runDB $ selectFora Nothing
    defaultLayout $ do
        let currentForumTitle = appForumTitle $ appSettings master
        setTitle $ toHtml currentForumTitle
        $(widgetFile "homepage")
