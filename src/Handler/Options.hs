{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Options
  ( getOptionsR
  , getOptionCreateFormR
  , postOptionsR
  , getOptionR
  , getOptionEditFormR
  , postOptionR
  , postOptionsDeleteR
  ) where

import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T (lines)
import qualified Data.List.Safe as LS (head)
import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage), preEscapedToMarkup)
import Yesod.Core.Handler (HandlerFor, redirect, getMessages, addMessageI, lookupPostParams)
import Yesod.Core.Widget (WidgetFor, setTitleI, whamlet)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvLabel, fvInput, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (textField, textareaField, doubleField, checkBoxField, intField, unTextarea)

import Foundation
    ( App
    , Route (AdminR)
    , AdminR (StemR, OptionCreateFormR, OptionsR, OptionR, OptionEditFormR, OptionsDeleteR)
    , AppMessage
      ( MsgOptions, MsgBack, MsgAnswerOptions, MsgAdd, MsgOrdinal, MsgOption
      , MsgCancel, MsgSave, MsgText, MsgCorrectAnswer, MsgAnswerPoints
      , MsgAnswerOption, MsgKey, MsgDistractor, MsgEdit, MsgDelete
      , MsgType, MsgQuestion, MsgExam, MsgSkill, MsgDuplicateValue
      , MsgInvalidData, MsgClose, MsgPleaseConfirm, MsgAreYouSureDelete
      , MsgRecordDeleted, MsgRecordEdited, MsgNewRecordAdded
      )
    )

import Yesod.Persist (YesodPersist(runDB), Entity (entityVal), PersistStoreWrite (insert_, replace))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist (Entity (Entity))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&))
    , orderBy, asc, innerJoin, on, Value (unValue), not_, in_
    , delete, valList
    )

import Model
    ( TestId, StemId, Option (Option, optionOrdinal, optionText, optionKey, optionPoints)
    , EntityField
      ( OptionStem, OptionOrdinal, OptionId, StemId, StemTest, StemSkill
      , SkillId, TestId, StemText, TestName, SkillName
      )
    , OptionId, Stem, Test, Skill
    )
import Yesod.Form.Input (runInputGet, iopt)


postOptionsDeleteR :: TestId -> StemId -> HandlerFor App Html
postOptionsDeleteR eid qid = do
    oids <- (toSqlKey . read . unpack <$>) <$> lookupPostParams "id"
    runDB $ delete $ do
        x <- from $ table @Option
        where_ $ x ^. OptionId `in_` valList oids
    addMessageI "--mdc-theme-info" MsgRecordDeleted
    redirect $ AdminR $ OptionsR eid qid


postOptionR :: TestId -> StemId -> OptionId -> HandlerFor App Html
postOptionR eid qid oid = do
    option <- runDB $ selectOne $ do
        x <- from $ table @Option
        where_ $ x ^. OptionId ==. val oid
        return x
    ((fr,widget),enctype) <- runFormPost $ formOption eid qid option
    case fr of
      FormSuccess o -> do
          runDB $ replace oid o
          addMessageI "--mdc-theme-info" MsgRecordEdited
          redirect $ AdminR $ OptionR eid qid oid
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgAnswerOption
          $(widgetFile "options/edit")


getOptionEditFormR :: TestId -> StemId -> OptionId -> HandlerFor App Html
getOptionEditFormR eid qid oid = do
    option <- runDB $ selectOne $ do
        x <- from $ table @Option
        where_ $ x ^. OptionId ==. val oid
        return x
    (widget,enctype) <- generateFormPost $ formOption eid qid option
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgAnswerOption
        $(widgetFile "options/edit")


getOptionR :: TestId -> StemId -> OptionId -> HandlerFor App Html
getOptionR eid qid oid = do
    option <- ((\(x,q,e,s) -> (x,unValue q,unValue e,unValue s)) <$>) <$> runDB (selectOne $ do
        (x :& q :& e :& s) <- from $ table @Option
            `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
            `innerJoin` table @Test `on` (\(_ :& q :& e) -> q ^. StemTest ==. e ^. TestId)
            `innerJoin` table @Skill `on` (\(_ :& q :& _ :& s) -> q ^. StemSkill ==. s ^. SkillId)
        where_ $ x ^. OptionId ==. val oid
        return (x, q ^. StemText, e ^. TestName, s ^. SkillName))
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAnswerOption
        $(widgetFile "options/option")


postOptionsR :: TestId -> StemId -> HandlerFor App Html
postOptionsR eid qid = do
    ((fr,widget),enctype) <- runFormPost $ formOption eid qid Nothing
    case fr of
      FormSuccess x -> do
          runDB $ insert_ x
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirect $ AdminR $ OptionsR eid qid
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgOption
          $(widgetFile "options/create")


getOptionsR :: TestId -> StemId -> HandlerFor App Html
getOptionsR eid qid = do
    moid <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    options <- runDB $ select $ do
        x <- from $ table @Option
        where_ $ x ^. OptionStem ==. val qid
        orderBy [asc (x ^. OptionOrdinal)]
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOptions
        $(widgetFile "options/options")


getOptionCreateFormR :: TestId -> StemId -> HandlerFor App Html
getOptionCreateFormR eid qid = do
    (widget,enctype) <- generateFormPost $ formOption eid qid Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgOption
        $(widgetFile "options/create")


formOption :: TestId -> StemId -> Maybe (Entity Option)
           -> Html -> MForm (HandlerFor App) (FormResult Option, WidgetFor App ())
formOption _ qid option extra = do
    (ordinalR,ordinalV) <- mreq uniqueOrdinalField FieldSettings
        { fsLabel = SomeMessage MsgOrdinal
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (optionOrdinal . entityVal <$> option)
    (textR,textV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgText
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (optionText . entityVal <$> option)
    (keyR,keyV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgCorrectAnswer
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (optionKey . entityVal <$> option)
    (pointsR,pointsV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgAnswerPoints
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (optionPoints . entityVal <$> option)

    let r = Option qid <$> ordinalR <*> textR <*> keyR <*> pointsR
    let w = [whamlet|
#{extra}
<label.mdc-text-field.mdc-text-field--filled.form-field data-mdc-auto-init=MDCTextField
  :isJust (fvErrors ordinalV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel ordinalV}
  ^{fvInput ordinalV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors ordinalV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
      #{errs}

<label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea.form-field data-mdc-auto-init=MDCTextField
  :isJust (fvErrors textV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel textV}
  <span.mdc-text-field__resizer>
    ^{fvInput textV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors textV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
      #{errs}

$with checked <- fromMaybe False ((optionKey . entityVal) <$> option)
  <div.mdc-form-field.form-field data-mdc-auto-init=MDCFormField>
    <button.mdc-switch type=button role=switch aria-checked=false #switchKey data-mdc-auto-init=MDCSwitch
      :checked:.mdc-switch--selected :checked:aria-checked=true
      :not checked:.mdc-switch--unselected :not checked:aria-checked=false>
      ^{fvInput keyV}
      <div.mdc-switch__track>
      <div.mdc-switch__handle-track>
        <div.mdc-switch__handle>
          <div.mdc-switch__shadow>
            <div.mdc-elevation-overlay>
          <div.mdc-switch__ripple>
          <div.mdc-switch__icons>
            <svg.mdc-switch__icon.mdc-switch__icon--on viewBox="0 0 24 24">
              <path d="M19.69,5.23L8.96,15.96l-4.23-4.23L2.96,13.5l6,6L21.46,7L19.69,5.23z">
            <svg.mdc-switch__icon.mdc-switch__icon--off viewBox="0 0 24 24">
              <path d="M20 13H4v-2h16v2z">

    <span.mdc-switch__focus-ring-wrapper>
      <span.mdc-switch__focus-ring>
    <label for=switchKey>#{fvLabel keyV}

<label.mdc-text-field.mdc-text-field--filled.form-field data-mdc-auto-init=MDCTextField
  :isJust (fvErrors pointsV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel pointsV}
  ^{fvInput pointsV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors pointsV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
      #{errs}
|]
    return (r,w)

  where

      uniqueOrdinalField = checkM uniqueOrdinal textField

      uniqueOrdinal :: Text -> HandlerFor App (Either AppMessage Text)
      uniqueOrdinal ordinal = do
          mo <- runDB $ selectOne $ do
              x <- from $ table @Option
              where_ $ x ^. OptionStem ==. val qid
              where_ $ x ^. OptionOrdinal ==. val ordinal
              case option of
                Nothing -> return ()
                Just (Entity oid _) -> where_ $ not_ $ x ^. OptionId ==. val oid
              return x
          return $ case mo of
            Nothing -> Right ordinal
            _ -> Left MsgDuplicateValue
