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

import Control.Applicative ( Alternative((<|>)) )

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.Safe as LS (head)
import Data.Text (pack)
import qualified Data.Text as T (lines)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, orderBy, asc
    , (^.), (==.), (:&)((:&))
    , val, not_, max_, Value (unValue), delete
    , innerJoin, on
    )
import Database.Persist (Entity (Entity, entityVal), insert_, replace)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App, Handler, Form, widgetSnackbar
    , Route (DataR)
    , DataR
      ( TestR, StemsR, StemR, OptionsR, StemEditFormR, StemCreateFormR
      , StemsDeleteR
      )
    , AppMessage
      ( MsgQuestions, MsgQuestion, MsgAdd, MsgOrdinal, MsgText
      , MsgSave, MsgCancel, MsgSkill, MsgDelete, MsgInvalidData
      , MsgDuplicateValue
      , MsgOptions, MsgBack, MsgExam, MsgInstruction, MsgType
      , MsgMultiResponse, MsgSingleResponse, MsgResponseOptions
      , MsgNewRecordAdded, MsgRecordDeleted, MsgRecordEdited, MsgEdit
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgInvalidFormData
      )
    )

import Material3 (md3widget, md3textareaWidget, md3selectWidget)

import Model
    ( msgSuccess, msgError
    , TestId, Test (Test, testName)
    , StemId
    , Stem (Stem, stemOrdinal, stemText, stemSkill, stemInstruc, stemType)
    , Skill (skillName)
    , StemType (SingleRespose, MultiResponse)
    , EntityField
      ( TestId, StemOrdinal, StemTest, SkillName, StemId, StemSkill
      , SkillId
      )
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( defaultLayout, SomeMessage (SomeMessage), whamlet
    , MonadHandler (liftHandler)
    , redirect, getMessages, addMessageI, preEscapedToMarkup, newIdent
    )
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (intField, textareaField, unTextarea, selectFieldList)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, checkM)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (runDB)


getStemR :: TestId -> StemId -> HandlerFor App Html
getStemR eid qid = do
    stem <- runDB $ selectOne $ do
        x :& e :& s <- from $ table @Stem
            `innerJoin` table @Test `on` (\(x :& e) -> x ^. StemTest ==. e ^. TestId)
            `innerJoin` table @Skill `on` (\(x :& _ :& s) -> x ^. StemSkill ==. s ^. SkillId)
        where_ $ x ^. StemId ==. val qid
        where_ $ x ^. StemTest ==. val eid
        return (x,e,s)

    (fw0,et0) <- generateFormPost formStemDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuestion
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "stems/stem") 


formStemDelete :: Form ()
formStemDelete extra = return (pure (), [whamlet|^{extra}|])


postStemsDeleteR :: TestId -> StemId -> Handler Html
postStemsDeleteR eid qid = do
    ((fr,_),_) <- runFormPost formStemDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Stem
              where_ $ x ^. StemId ==. val qid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ StemsR eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ StemR eid qid


postStemR :: TestId -> StemId -> Handler Html
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
          redirect $ DataR $ StemR eid qid
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


getStemCreateFormR :: TestId -> Handler Html
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


postStemsR :: TestId -> Handler Html
postStemsR eid = do
    ((fr,widget),enctype) <- runFormPost $ formStem eid Nothing
    case fr of
      FormSuccess q -> do
          runDB $ insert_ q
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirect $ DataR $ StemsR eid
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


getStemsR :: TestId -> Handler Html
getStemsR eid = do
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


formStem :: TestId -> Maybe (Entity Stem) -> Form Stem
formStem eid stem extra = do
    skills <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Skill
        orderBy [asc (x ^. SkillName), asc (x ^. SkillId)]
        return (x ^. SkillName, x ^. SkillId) )
        
    (skillR,skillV) <- mreq (selectFieldList skills) FieldSettings
        { fsLabel = SomeMessage MsgSkill
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (stemSkill . entityVal <$> stem)

    ordinal <- case stem of
      Nothing -> do
          n <- liftHandler $ runDB $ ((+ 1) <$>) . (unValue =<<) <$> selectOne ( do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val eid
              return $ max_ $ x ^. StemOrdinal)
          return $ n <|> pure 1
          
      _otherwise -> return Nothing
        
    (ordinalR,ordinalV) <- mreq uniqueOrdinalField FieldSettings
        { fsLabel = SomeMessage MsgOrdinal
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing , fsAttrs = []
        } ((stemOrdinal . entityVal <$> stem) <|> ordinal)
        
    (textR,textV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgText
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (stemText . entityVal <$> stem)

    let types = [(MsgSingleResponse, SingleRespose),(MsgMultiResponse, MultiResponse)]
    
    (typeR,typeV) <- mreq (selectFieldList types) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (stemType . entityVal <$> stem)
        
    (instrucR,instrucV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgInstruction
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (stemInstruc . entityVal <$> stem)

    let r = Stem eid <$> skillR <*> ordinalR <*> textR <*> typeR <*> instrucR
    let w = [whamlet|
                    #{extra}

                    ^{md3widget ordinalV}

                    ^{md3textareaWidget textV}

                    ^{md3selectWidget typeV}

                    ^{md3textareaWidget instrucV}

                    ^{md3selectWidget skillV}
                    |]
    return (r,w)
  where

      uniqueOrdinalField = checkM uniqueOrdinal intField

      uniqueOrdinal :: Int -> Handler (Either AppMessage Int)
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
