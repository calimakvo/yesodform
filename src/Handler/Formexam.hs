{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Formexam where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..),
                              renderBootstrap3)

data FileForm = FileForm {
    fileInfo :: FileInfo
  , inpText :: Text
}

getFormexamR :: Handler Html
getFormexamR = do
  (formAWidgetBasic, formAEnctypeBasic) <- generateFormPost fileUploadAFormBasic
  (formAWidgetInline, formAEnctypeInline) <- generateFormPost fileUploadAFormInline
  (formAWidgetHorizon, formAEnctypeHorizon) <- generateFormPost fileUploadAFormHorizon
  ((res, widget), enctype) <- runFormPost fileUploadMForm
  defaultLayout $ do
        setTitle "Form Example!!!"
        $(widgetFile "formexam")

postFormexamR :: Handler Html
postFormexamR = error "Not yet implemented: postFormexamR"

fileUploadMForm :: Html -> MForm Handler (FormResult FileForm, Widget)
fileUploadMForm extra = do
    let fsMedia = FieldSettings {
            fsLabel = ""
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Just "media"
          , fsAttrs =
              [   ("class", "uploadFile")
                , ("style", "display:none")
                , ("placeholder", "")
              ]
        }
    let fsText = FieldSettings {
            fsLabel = "なんか"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Just "inptext"
          , fsAttrs =
              [   ("class", "form-group form-control")
                , ("placeholder", "")
              ]
        }
        
    (fileInfoRes, fileInfoView) <- mreq fileField fsMedia Nothing
    (inpTextRes, inpTextView) <- mreq textField fsText Nothing
    let fileRes = FileForm <$> fileInfoRes <*> inpTextRes
    let widget = do
            [whamlet|
                #{extra}
                <label>
                    <div class="form-group">
                        <span class="btn btn-default">Upload
                            ^{fvInput fileInfoView}
                    ^{fvInput inpTextView}
            |]
    return (fileRes, widget)

fileUploadAFormInline :: Form FileForm
fileUploadAFormInline = renderBootstrap3 BootstrapInlineForm $ getAform

fileUploadAFormBasic :: Form FileForm
fileUploadAFormBasic = renderBootstrap3 BootstrapBasicForm $ getAform

fileUploadAFormHorizon :: Form FileForm
fileUploadAFormHorizon = renderBootstrap3 (BootstrapHorizontalForm (ColXs 2) (ColXs 8) (ColXs 2) (ColXs 6)) $ getAform

getAform :: AForm Handler FileForm
getAform = FileForm
    <$> fileAFormReq fs <*> areq textField tfs Nothing
    where fs = FieldSettings {
            fsLabel = "ファイル"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Just "media"
          , fsAttrs =
              [   ("class", "form-group")
                , ("placeholder", "File description")
              ]
          }
          tfs = FieldSettings {
            fsLabel = "なんかいれろ"
          , fsTooltip = Nothing
          , fsId = Just "inptext"
          , fsName = Just "inptext"
          , fsAttrs =
              [   ("class", "form-group form-control")
                , ("placeholder", "input text")
              ]
          }
