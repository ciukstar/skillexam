{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Tests
  ( getTestsR
  , getTestCreateFormR
  , postTestsR
  , getTestEditFormR
  , postTestR
  , getTestR
  , postTestsDeleteR
  , getTestSearchR
  , postTestPublishR
  , postTestUnpublishR
  ) where

import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, unpack, pack)
import Text.Hamlet (Html)
import qualified Text.Printf as Printf (printf)
import Settings (widgetFile)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))
import Yesod.Core (defaultLayout, setTitleI, redirect, addMessageI, getMessages)
import Yesod.Core.Handler
    ( HandlerFor, lookupPostParams, lookupGetParam, lookupPostParam
    , lookupSession, setUltDestCurrent
    )
import Yesod.Core.Widget (WidgetFor, whamlet)
import Yesod.Form.Types
    ( MForm, FormResult
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvLabel, fvInput, fvErrors)
    , FormResult (FormSuccess)
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (Textarea (Textarea), textField, textareaField, doubleField, intField, hiddenField)
import Yesod.Form.Input (runInputGet, iopt)

import Foundation
    ( App
    , Route (AdminR, SignInR, SignOutR, PhotoPlaceholderR)
    , AdminR
      ( TestsR, TestR, TestsDeleteR, TestSearchR, TestEditFormR, TestCreateFormR
      , StemsR, TestPublishR, TestUnpublishR, CandidatePhotoR
      )
    , AppMessage
      ( MsgTests, MsgSearch, MsgAdd, MsgBack, MsgCode
      , MsgName, MsgDescr, MsgTest, MsgCancel, MsgSave, MsgInvalidData
      , MsgDuplicateValue, MsgEdit, MsgDelete, MsgPleaseConfirm
      , MsgAreYouSureDelete, MsgQuestions, MsgDuration, MsgPassMark
      , MsgMinutes, MsgPoints, MsgLogin, MsgLogout, MsgPhoto, MsgExamQuestions
      , MsgNewRecordAdded, MsgRecordDeleted, MsgRecordEdited, MsgExamState
      , MsgPublished, MsgUnpublished, MsgPublish, MsgUnpublish
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Persist (Entity (Entity), entityVal, PersistStoreWrite (insert_, replace))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    (select, from, table, where_, orderBy, desc
    , (^.), (==.), (%), (++.), (||.), (=.)
    , val, selectOne, delete, in_, valList, upper_
    , like, just, update, set, Value (unValue)
    )

import Model
    ( TestId, Test (testCode, testName, testDescr, Test, testDuration, testPass, testState)
    , EntityField (TestId, TestCode, TestName, TestDescr, CandidateId, TestState)
    , Candidate (Candidate), userSessKey, TestState (TestStatePublished, TestStateUnpublished)
    )


printf :: String -> Double -> String
printf = Printf.printf


postTestPublishR :: TestId -> HandlerFor App Html
postTestPublishR eid = do
    state <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return (x ^. TestState) )
    ((fr,_),_) <- runFormPost $ formTestStateToggle state
    case fr of
      FormSuccess _ -> do
          runDB $ update $ \x -> do
              set x [TestState =. val TestStatePublished]
              where_ $ x ^. TestId ==. val eid
          redirect $ AdminR $ TestR eid
      _ -> redirect $ AdminR $ TestR eid


postTestUnpublishR :: TestId -> HandlerFor App Html
postTestUnpublishR eid = do
    state <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return (x ^. TestState) )
    ((fr,_),_) <- runFormPost $ formTestStateToggle state
    case fr of
      FormSuccess _ -> do
          runDB $ update $ \x -> do
              set x [TestState =. val TestStateUnpublished]
              where_ $ x ^. TestId ==. val eid
          redirect $ AdminR $ TestR eid
      _ -> redirect $ AdminR $ TestR eid


formTestStateToggle :: Maybe TestState -> Html -> MForm (HandlerFor App) (FormResult TestState, WidgetFor App ())
formTestStateToggle state extra = do
    (r,v) <- first (read . unpack <$>) <$> mreq hiddenField "" (pack . show <$> state)
    let w = [whamlet|#{extra} ^{fvInput v}|]
    return (r,w)


getTestSearchR :: HandlerFor App Html
getTestSearchR = do
    mq <- lookupGetParam "q"
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    tests <- runDB $ select $ do
        x <- from $ table @Test
        case mq of
          Nothing -> return ()
          Just q -> where_ $ (upper_ (x ^. TestCode) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. TestName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. TestDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%))
        orderBy [desc (x ^. TestId)]
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "tests/search")
        $(widgetFile "tests/tests")


postTestsDeleteR :: HandlerFor App Html
postTestsDeleteR = do
    eids <- (toSqlKey . read . unpack <$>) <$> lookupPostParams "id"
    runDB $ delete $ do
        x <- from $ table @Test
        where_ $ x ^. TestId `in_` valList eids
    addMessageI "--mdc-theme-info" MsgRecordDeleted
    redirect $ AdminR TestsR


getTestR :: TestId -> HandlerFor App Html
getTestR eid = do
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    (widget,enctype) <- generateFormPost $ formTestStateToggle (testState . entityVal <$> test)
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTest
        $(widgetFile "tests/test")


postTestR :: TestId -> HandlerFor App Html
postTestR eid = do
    scrollY <- fromMaybe "0" <$> lookupPostParam "scrollY"
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    ((fr,widget),enctype) <- runFormPost $ formTest test
    case fr of
      FormSuccess x -> do
          runDB $ replace eid x
          addMessageI "--mdc-theme-info" MsgRecordEdited
          redirect $ AdminR $ TestR eid
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgTest
          $(widgetFile "tests/edit")


getTestEditFormR :: TestId -> HandlerFor App Html
getTestEditFormR eid = do
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    (widget, enctype) <- generateFormPost $ formTest test
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgTest
        $(widgetFile "tests/edit")


postTestsR :: HandlerFor App Html
postTestsR = do
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    ((fr,widget),enctype) <- runFormPost $ formTest Nothing
    case fr of
      FormSuccess test -> do
          runDB $ insert_ test
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirect $ AdminR TestsR
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgTest
          $(widgetFile "tests/create")


getTestCreateFormR :: HandlerFor App Html
getTestCreateFormR = do
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    (widget,enctype) <- generateFormPost $ formTest Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgTest
        $(widgetFile "tests/create")


getTestsR :: HandlerFor App Html
getTestsR = do
    mcid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    candidate <- case mcid of
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Candidate
          where_ $ x ^. CandidateId ==. val cid
          return x
      _ -> return Nothing
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    tests <- runDB $ select $ do
        x <- from $ table @Test
        orderBy [desc (x ^. TestId)]
        return x
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTests
        $(widgetFile "tests/main")
        $(widgetFile "tests/tests")


formTest :: Maybe (Entity Test) -> Html -> MForm (HandlerFor App) (FormResult Test, WidgetFor App ())
formTest test extra = do
    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (testCode . entityVal <$> test)
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (testName . entityVal <$> test)
    (durationR,durationV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (testDuration . entityVal <$> test)
    (passR,passV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPassMark
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (testPass . entityVal <$> test)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescr
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (testDescr . entityVal <$> test)
    (stateR,stateV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgExamState
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (pack . show <$> ((testState . entityVal <$> test) <|> pure TestStateUnpublished))

    let r = Test <$> codeR <*> nameR <*> durationR <*> passR <*> descrR <*> stateR

    let w = [whamlet|
#{extra}
$forall v <- [codeV,nameV]
  <label.mdc-text-field.mdc-text-field--filled.mt-1 data-mdc-auto-init=MDCTextField
    :isJust (fvErrors v):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel v}
    ^{fvInput v}
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors v
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
        
<label.mdc-text-field.mdc-text-field--filled.mt-1 data-mdc-auto-init=MDCTextField
  :isJust (fvErrors durationV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel durationV}
  ^{fvInput durationV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors durationV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
      #{errs}. _{MsgMinutes}
$nothing
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--persistent aria-hidden=false>
      _{MsgMinutes}
      
<label.mdc-text-field.mdc-text-field--filled.mt-1 data-mdc-auto-init=MDCTextField
  :isJust (fvErrors passV):.mdc-text-field--invalid>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>#{fvLabel passV}
  ^{fvInput passV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors passV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
      #{errs}. _{MsgPoints}
$nothing
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--persistent aria-hidden=false>
      _{MsgPoints}

<label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea.mt-1 data-mdc-auto-init=MDCTextField
  :isJust (fvErrors descrV):.mdc-text-field--invalid>
  <span.mdc-text-field__riple>
  <span.mdc-floating-label>#{fvLabel descrV}
  <span.mdc-text-field__resizer>
    ^{fvInput descrV}
  <span.mdc-line-ripple>
$maybe errs <- fvErrors descrV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
      #{errs}

<div.mdc-select.mdc-select--filled.mdc-select--required.mt-1 data-mdc-auto-init=MDCSelect
  :isJust (fvErrors stateV):.mdc-select--invalid>
  ^{fvInput stateV}
  <div.mdc-select__anchor role=button aria-aspopup=listbox aria-expanded=false aria-required=true>
    <span.mdc-select__ripple>
    <span.mdc-floating-label>#{fvLabel stateV}
    <span.mdc-select__selected-text-container>
      <span.mdc-select__selected-text>
    <span.mdc-select__dropdown-icon>
      <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
        <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
        <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
    <span.mdc-line-ripple>
    

  <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
    <ul.mdc-deprecated-list role=listbox>
      $forall (v,l) <- ((<$>) (first (pack . show)) [(TestStatePublished,MsgPublished),(TestStateUnpublished,MsgUnpublished)])
        <li.mdc-deprecated-list-item role=option data-value=#{v} aria-selected=false>
          <span.mdc-deprecated-list-item__ripple>
          <span.mdc-deprecated-list-item__text>
            _{l}
$maybe errs <- fvErrors stateV
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
      #{errs}
|]
    return (r,w)
  where

    uniqueCodeField = checkM uniqueCode textField

    uniqueCode :: Text -> HandlerFor App (Either AppMessage Text)
    uniqueCode code = do
        mx <- runDB $ selectOne $ do
            x <- from $ table @Test
            where_ $ x ^. TestCode ==. val code
            return x
        return $ case mx of
          Nothing -> Right code
          Just (Entity eid _) -> case test of
            Nothing -> Left MsgDuplicateValue
            Just (Entity eid' _) | eid' == eid -> Right code
                                 | otherwise -> Left MsgDuplicateValue
