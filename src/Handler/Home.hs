module Handler.Home where

import Import
import Partials

getHomeR :: Handler Html
getHomeR = do
    master <- getYesod
    muser <- maybeAuth
    fora <- runDB $ selectList [ForumParent ==. Nothing] [Desc ForumTitle]
    defaultLayout $ do
        setTitle . toHtml . appForumTitle $ appSettings master
        $(widgetFile "homepage")
