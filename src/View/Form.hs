module View.Form where

import Import
import Yesod.Form.Bootstrap3

mkFormView :: Route App -> Maybe Widget -> Enctype -> Widget
mkFormView url mwidget enctype = case mwidget of
    Just widget -> [whamlet|
        <form .form-horizontal role=form method=post action=@{url} enctype=#{enctype}>
            ^{widget}
        |]
    _ -> mempty


extendFieldSettings :: Text -> Text -> FieldSettings site -> FieldSettings site
extendFieldSettings attr val fs = fs { fsAttrs = newAttrs }
    where newAttrs = (attr, val) : fsAttrs fs

renderForm :: Monad m => FormRender m a
renderForm = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 4) (ColMd 0) (ColMd 6))
