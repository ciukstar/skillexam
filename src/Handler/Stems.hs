{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Stems
  ( getStemsR
  , getStemCreateFormR
  , postStemsR
  , getStemEditFormR
  , postStemR
  , postStemsDeleteR
  , getStemR
  ) where

import qualified Data.List.Safe as LS (head)
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe (isJust, fromMaybe)
import Data.Text (unpack, pack)
import qualified Data.Text as T (lines)
import Text.Hamlet (Html)
import Data.Bifunctor (Bifunctor(first))
import Yesod.Core
    ( defaultLayout, SomeMessage (SomeMessage), whamlet
    , MonadHandler (liftHandler)
    , redirect, getMessages, addMessageI, lookupPostParams
    , preEscapedToMarkup
    )
import Yesod.Core.Handler (HandlerFor)
import Settings (widgetFile)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, checkM)
import Yesod.Form.Fields (intField, textareaField, hiddenField, unTextarea, textField)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvErrors)
    )
import Yesod.Core.Widget (WidgetFor, setTitleI)

import Foundation
    ( App
    , Route (AdminR)
    , AdminR (TestR, StemsR, StemR, OptionsR, StemEditFormR, StemCreateFormR, StemsDeleteR)
    , AppMessage
      ( MsgQuestions, MsgQuestion, MsgAdd, MsgOrdinal, MsgText
      , MsgSave, MsgCancel, MsgSkill, MsgDelete, MsgInvalidData
      , MsgDuplicateValue, MsgPleaseConfirm, MsgAreYouSureDelete
      , MsgOptions, MsgBack, MsgExam, MsgInstruction, MsgType
      , MsgMultiResponse, MsgSingleResponse, MsgResponseOptions
      , MsgNewRecordAdded, MsgRecordDeleted, MsgRecordEdited
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Persist (Entity (Entity, entityVal), PersistStoreWrite (insert_, replace))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, orderBy, asc
    , (^.), (==.), (:&)((:&))
    , val, not_, max_, Value (unValue), valList, in_, delete
    , innerJoin, on
    )

import Model
    ( TestId, Test (Test)
    , StemId, Stem (Stem, stemOrdinal, stemText, stemSkill, stemInstruc, stemType)
    , EntityField (TestId, StemOrdinal, StemTest, SkillName, StemId, StemSkill, SkillId)
    , Skill (Skill), StemType (SingleRespose, MultiResponse)
    )
import Yesod.Form.Input (runInputGet, iopt)


getStemR :: TestId -> StemId -> HandlerFor App Html
getStemR eid qid = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    stem <- runDB $ selectOne $ do
        x :& e :& s <- from $ table @Stem
            `innerJoin` table @Test `on` (\(x :& e) -> x ^. StemTest ==. e ^. TestId)
            `innerJoin` table @Skill `on` (\(x :& _ :& s) -> x ^. StemSkill ==. s ^. SkillId)
        where_ $ x ^. StemId ==. val qid
        where_ $ x ^. StemTest ==. val eid
        return (x,e,s)
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuestion
        $(widgetFile "stems/stem")


postStemsDeleteR :: TestId -> HandlerFor App Html
postStemsDeleteR eid = do
    qids <- (toSqlKey . read . unpack <$>) <$> lookupPostParams "id"
    runDB $ delete $ do
        x <- from $ table @Stem
        where_ $ x ^. StemId `in_` valList qids
    addMessageI "--mdc-theme-info" MsgRecordDeleted
    redirect $ AdminR $ StemsR eid 


postStemR :: TestId -> StemId -> HandlerFor App Html
postStemR eid qid = do
    question <- runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemId ==. val qid
        return x
    ((fr,widget),enctype) <- runFormPost $ formStem eid question
    case fr of
      FormSuccess q -> do
          runDB $ replace qid q
          addMessageI "--mdc-theme-info" MsgRecordEdited
          redirect $ AdminR $ StemR eid qid
      _ -> defaultLayout $ do
          setTitleI MsgQuestion
          $(widgetFile "stems/edit")


getStemEditFormR :: TestId -> StemId -> HandlerFor App Html
getStemEditFormR eid qid = do
    question <-runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemId ==. val qid
        where_ $ x ^. StemTest ==. val eid
        return x
    (widget,enctype) <- generateFormPost $ formStem eid question
    defaultLayout $ do
        setTitleI MsgQuestion
        $(widgetFile "stems/edit")


getStemCreateFormR :: TestId -> HandlerFor App Html
getStemCreateFormR eid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    (widget, enctype) <- generateFormPost $ formStem eid Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgQuestion
        $(widgetFile "stems/create")


postStemsR :: TestId -> HandlerFor App Html
postStemsR eid = do
    ((fr,widget),enctype) <- runFormPost $ formStem eid Nothing
    case fr of
      FormSuccess q -> do
          runDB $ insert_ q
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirect $ AdminR $ StemsR eid
      _ -> do
          test <- runDB $ selectOne $ do
              x <- from $ table @Test
              where_ $ x ^. TestId ==. val eid
              return x
          defaultLayout $ do
              addMessageI "--mdc-theme-error" MsgInvalidData
              msgs <- getMessages
              setTitleI MsgQuestion
              $(widgetFile "stems/create")


getStemsR :: TestId -> HandlerFor App Html
getStemsR eid = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    stems <- runDB $ select $ do
        x <- from $ table @Stem
        where_ $ x ^. StemTest ==. val eid
        orderBy [asc (x ^. StemOrdinal)]
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuestions
        $(widgetFile "stems/main")
        $(widgetFile "stems/stems")


formStem :: TestId
         -> Maybe (Entity Stem)
         -> Html -> MForm (HandlerFor App) (FormResult Stem, WidgetFor App ())
formStem eid stem extra = do
    skills <- liftHandler $ runDB $ select $ do
        x <- from $ table @Skill
        orderBy [asc (x ^. SkillName)]
        return x
        
    (skillR,skillV) <- first (toSqlKey <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgSkill
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-select mdc-select--filled")]
        } (fromSqlKey . stemSkill . entityVal <$> stem)

    ordinal <- case stem of
      Nothing -> do
          n <- liftHandler $ runDB $ ((+ 1) <$>) . (unValue =<<) <$> selectOne ( do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val eid
              return $ max_ $ x ^. StemOrdinal)
          return $ n <|> pure 1
      _ -> return Nothing
        
    (ordinalR,ordinalV) <- mreq uniqueOrdinalField FieldSettings
        { fsLabel = SomeMessage MsgOrdinal
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((stemOrdinal . entityVal <$> stem) <|> ordinal)
        
    (textR,textV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgText
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (stemText . entityVal <$> stem)
    
    (typeR,typeV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = []
        } (pack . show . stemType . entityVal <$> stem)
        
    (instrucR,instrucV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgInstruction
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (stemInstruc . entityVal <$> stem)

    let r = Stem eid <$> skillR <*> ordinalR <*> textR <*> typeR <*> instrucR
    let w = [whamlet|
#{extra}

<label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
  :isJust (fvErrors ordinalV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel ordinalV}
  ^{fvInput ordinalV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors ordinalV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
      #{errs}

<label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
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

<div.mdc-select.mdc-select--filled.mdc-select--required data-mdc-auto-init=MDCSelect
  :isJust (fvErrors typeV):.mdc-select--invalid>
  ^{fvInput typeV}
  <div.mdc-select__anchor role=button aria-aspopup=listbox aria-expanded=false aria-required=true>
    <span.mdc-select__ripple>
    <span.mdc-floating-label>#{fvLabel typeV}
    <span.mdc-select__selected-text-container>
      <span.mdc-select__selected-text>
    <span.mdc-select__dropdown-icon>
      <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
        <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
        <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
    <span.mdc-line-ripple>
    

  <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
    <ul.mdc-deprecated-list role=listbox>
      $forall (v,l) <- ((<$>) (first (pack . show)) [(SingleRespose,MsgSingleResponse),(MultiResponse,MsgMultiResponse)])
        <li.mdc-deprecated-list-item role=option data-value=#{v} aria-selected=false>
          <span.mdc-deprecated-list-item__ripple>
          <span.mdc-deprecated-list-item__text>
            _{l}

$maybe errs <- fvErrors skillV
  <div.mdc-select-helper-text.mdc-select-helper-text--validation-msg>
    #{errs}

<label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
  :isJust (fvErrors instrucV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel instrucV}
  <span.mdc-text-field__resizer>
    ^{fvInput instrucV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors instrucV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
      #{errs}

<div.mdc-select.mdc-select--filled.mdc-select--required data-mdc-auto-init=MDCSelect
  :isJust (fvErrors skillV):.mdc-select--invalid>
  ^{fvInput skillV}
  <div.mdc-select__anchor role=button aria-aspopup=listbox aria-expanded=false aria-required=true>
    <span.mdc-select__ripple>
    <span.mdc-floating-label>#{fvLabel skillV}
    <span.mdc-select__selected-text-container>
      <span.mdc-select__selected-text>
    <span.mdc-select__dropdown-icon>
      <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
        <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
        <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
    <span.mdc-line-ripple>
    

  <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
    <ul.mdc-deprecated-list role=listbox>
      $forall Entity sid (Skill _ name _) <- skills
        <li.mdc-deprecated-list-item role=option data-value=#{fromSqlKey sid} aria-selected=false>
          <span.mdc-deprecated-list-item__ripple>
          <span.mdc-deprecated-list-item__text>
            #{name}

$maybe errs <- fvErrors skillV
  <div.mdc-select-helper-text.mdc-select-helper-text--validation-msg>
    #{errs}

|]
    return (r,w)
  where

      uniqueOrdinalField = checkM uniqueOrdinal intField

      uniqueOrdinal :: Int -> HandlerFor App (Either AppMessage Int)
      uniqueOrdinal n = do
          
          mq <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val eid
              where_ $ x ^. StemOrdinal ==. val n
              case stem of
                Nothing -> return ()
                Just (Entity qid _) -> where_ $ not_ $ x ^. StemId ==. val qid

          return $ case mq of
            Nothing -> Right n
            Just _ -> Left MsgDuplicateValue
