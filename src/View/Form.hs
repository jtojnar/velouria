module View.Form where

import Import

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
