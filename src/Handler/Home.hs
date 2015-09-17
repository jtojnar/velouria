module Handler.Home where

import Import
import Handler.ForumView (forumListPartial, selectFora)

getHomeR :: Handler Html
getHomeR = do
    master <- getYesod
    muser <- maybeAuth
    fora <- runDB $ selectFora Nothing
    defaultLayout $ do
        setTitle . toHtml . appForumTitle $ appSettings master
        $(widgetFile "homepage")
