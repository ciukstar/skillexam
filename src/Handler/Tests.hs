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
  , postTestDeleR
  , getTestSearchR
  , postTestPublishR
  , postTestUnpublishR
  ) where

import Control.Applicative (Alternative ((<|>)))

import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (isJust)
import Data.Text (Text, unpack, pack)

import Database.Esqueleto.Experimental
    (select, from, table, where_, orderBy, desc
    , (^.), (==.), (%), (++.), (||.), (=.)
    , val, selectOne, delete, upper_
    , like, just, update, set, Value (unValue)
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App, Form, Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (AdminR)
    , AdminR
      ( TestsR, TestR, TestDeleR, TestSearchR, TestEditFormR, TestCreateFormR
      , StemsR, TestPublishR, TestUnpublishR
      )
    , AppMessage
      ( MsgTests, MsgSearch, MsgAdd, MsgBack, MsgCode
      , MsgName, MsgDescr, MsgTest, MsgCancel, MsgSave, MsgInvalidData
      , MsgDuplicateValue, MsgEdit, MsgDelete
      , MsgQuestions, MsgDuration, MsgPassMark
      , MsgMinutes, MsgPoints, MsgExamQuestions
      , MsgNewRecordAdded, MsgRecordDeleted, MsgRecordEdited, MsgExamState
      , MsgPublished, MsgUnpublished, MsgPublish, MsgUnpublish, MsgNoTestsYet
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgInvalidFormData
      )
    )

import Material3 (md3widget, md3textareaWidget, md3selectWidget)

import Model
    ( msgSuccess, msgError
    , TestId
    , Test (testCode, testName, testDescr, Test, testDuration, testPass, testState)
    , EntityField (TestId, TestCode, TestName, TestDescr, TestState)
    , TestState (TestStatePublished, TestStateUnpublished)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import qualified Text.Printf as Printf (printf)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core (defaultLayout, setTitleI, redirect, addMessageI, getMessages, newIdent)
import Yesod.Core.Handler
    ( HandlerFor, lookupGetParam, setUltDestCurrent
    )
import Yesod.Core.Widget (WidgetFor, whamlet)
import Yesod.Form.Fields
    ( Textarea (Textarea), textField, textareaField, doubleField, intField
    , hiddenField, selectFieldList
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost, checkM)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    ( MForm, FormResult
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvLabel, fvInput, fvErrors, fvRequired, fvId)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (runDB)


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
          
      _otherwise -> redirect $ AdminR $ TestR eid


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
          
      _otherwise -> redirect $ AdminR $ TestR eid


formTestStateToggle :: Maybe TestState -> Html -> MForm (HandlerFor App) (FormResult TestState, WidgetFor App ())
formTestStateToggle state extra = do
    (r,v) <- first (read . unpack <$>) <$> mreq hiddenField "" (pack . show <$> state)
    let w = [whamlet|#{extra} ^{fvInput v}|]
    return (r,w)


getTestSearchR :: HandlerFor App Html
getTestSearchR = do
    mq <- lookupGetParam "q"
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
        idFormQuery <- newIdent
        $(widgetFile "data/tests/search")
        $(widgetFile "data/tests/tests")


postTestDeleR :: TestId -> HandlerFor App Html
postTestDeleR eid = do
    ((fr,_),_) <- runFormPost formTestDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Test
              where_ $ x ^. TestId ==. val eid
          
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ AdminR TestsR
          
      _otherwise -> do          
          addMessageI msgError MsgInvalidFormData
          redirect $ AdminR $ TestR eid


getTestR :: TestId -> HandlerFor App Html
getTestR eid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    (widget,enctype) <- generateFormPost $ formTestStateToggle (testState . entityVal <$> test)
    (fw0,et0) <- generateFormPost formTestDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTest
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/tests/test")


formTestDelete :: Form ()
formTestDelete extra = return (pure (), [whamlet|^{extra}|])


postTestR :: TestId -> Handler Html
postTestR eid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    ((fr,widget),enctype) <- runFormPost $ formTest test
    case fr of
      FormSuccess x -> do
          runDB $ replace eid x
          addMessageI msgSuccess MsgRecordEdited
          redirect $ AdminR $ TestR eid
          
      _otherwise -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgTest
          $(widgetFile "data/tests/edit")


getTestEditFormR :: TestId -> HandlerFor App Html
getTestEditFormR eid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x
    (widget, enctype) <- generateFormPost $ formTest test
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgTest
        $(widgetFile "data/tests/edit")


postTestsR :: HandlerFor App Html
postTestsR = do
    ((fr,widget),enctype) <- runFormPost $ formTest Nothing
    case fr of
      FormSuccess test -> do
          runDB $ insert_ test
          addMessageI msgSuccess MsgNewRecordAdded
          redirect $ AdminR TestsR
          
      _otherwise -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgTest
          $(widgetFile "data/tests/create")


getTestCreateFormR :: HandlerFor App Html
getTestCreateFormR = do
    (widget,enctype) <- generateFormPost $ formTest Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgTest
        $(widgetFile "data/tests/create")


getTestsR :: HandlerFor App Html
getTestsR = do
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
      
    tests <- runDB $ select $ do
        x <- from $ table @Test
        orderBy [desc (x ^. TestId)]
        return x
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTests
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/tests/main")
        $(widgetFile "data/tests/tests")


formTest :: Maybe (Entity Test) -> Form Test
formTest test extra = do
    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (testCode . entityVal <$> test)
        
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (testName . entityVal <$> test)
        
    (durationR,durationV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (testDuration . entityVal <$> test)
        
    (passR,passV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPassMark
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (testPass . entityVal <$> test)
        
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescr
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (testDescr . entityVal <$> test)

    let states = [(MsgUnpublished, TestStateUnpublished), (MsgPublished, TestStatePublished)]
        
    (stateR,stateV) <- mreq (selectFieldList states) FieldSettings
        { fsLabel = SomeMessage MsgExamState
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing , fsAttrs = []
        } ((testState . entityVal <$> test) <|> pure TestStateUnpublished)

    let r = Test <$> codeR <*> nameR <*> durationR <*> passR <*> descrR <*> stateR

    let w = [whamlet|
                    #{extra}
                    $forall v <- [codeV,nameV]
                      ^{md3widget v}

                    $forall (v,h) <- [(durationV,MsgMinutes),(passV,MsgPoints)]
                      <div.field.label.border.round.small :isJust (fvErrors v):.invalid>

                        ^{fvInput v}
                        <label for=#{fvId v}>
                          #{fvLabel v}
                          $if fvRequired v
                            <sup>*

                        $maybe err <- fvErrors v
                          <span.error>#{err}. _{h}
                        $nothing
                          <span.helper>_{h}


                    ^{md3textareaWidget descrV}

                    ^{md3selectWidget stateV}

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
