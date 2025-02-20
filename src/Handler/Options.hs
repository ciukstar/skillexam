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

import Data.Text (Text, pack)
import qualified Data.Text as T (lines)
import qualified Data.List.Safe as LS (head)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&))
    , orderBy, asc, innerJoin, on, Value (unValue), not_
    , delete
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar
    , Route (DataR)
    , DataR (StemR, OptionCreateFormR, OptionsR, OptionR, OptionEditFormR, OptionsDeleteR)
    , AppMessage
      ( MsgOptions, MsgBack, MsgAnswerOptions, MsgAdd, MsgOrdinal, MsgOption
      , MsgCancel, MsgSave, MsgText, MsgCorrectAnswer, MsgAnswerPoints
      , MsgAnswerOption, MsgKey, MsgDistractor, MsgEdit, MsgDelete
      , MsgType, MsgQuestion, MsgExam, MsgSkill, MsgDuplicateValue
      , MsgInvalidData, MsgClose
      , MsgRecordDeleted, MsgRecordEdited, MsgNewRecordAdded
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgInvalidFormData
      )
    )

import Material3 (md3widget, md3textareaWidget, md3switchWidget)

import Model
    ( TestId, StemId, Option (Option, optionOrdinal, optionText, optionKey, optionPoints)
    , EntityField
      ( OptionStem, OptionOrdinal, OptionId, StemId, StemTest, StemSkill
      , SkillId, TestId, StemText, TestName, SkillName
      )
    , OptionId, Stem, Test, Skill, msgSuccess, msgError
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage), preEscapedToMarkup, newIdent)
import Yesod.Core.Handler (redirect, getMessages, addMessageI)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (textField, textareaField, doubleField, checkBoxField, intField, unTextarea)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))


postOptionsDeleteR :: TestId -> StemId -> OptionId -> Handler Html
postOptionsDeleteR eid qid oid = do
    ((fr,_),_) <- runFormPost formOptionDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Option
              where_ $ x ^. OptionId ==. val oid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ OptionsR eid qid
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ OptionR eid qid oid


postOptionR :: TestId -> StemId -> OptionId -> Handler Html
postOptionR eid qid oid = do
    option <- runDB $ selectOne $ do
        x <- from $ table @Option
        where_ $ x ^. OptionId ==. val oid
        return x
    ((fr,widget),enctype) <- runFormPost $ formOption eid qid option
    case fr of
      FormSuccess o -> do
          runDB $ replace oid o
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ OptionR eid qid oid
      _ -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgAnswerOption
          $(widgetFile "options/edit")


getOptionEditFormR :: TestId -> StemId -> OptionId -> Handler Html
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


getOptionR :: TestId -> StemId -> OptionId -> Handler Html
getOptionR eid qid oid = do
    option <- ((\(x,q,e,s) -> (x,unValue q,unValue e,unValue s)) <$>) <$> runDB (selectOne $ do
        (x :& q :& e :& s) <- from $ table @Option
            `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
            `innerJoin` table @Test `on` (\(_ :& q :& e) -> q ^. StemTest ==. e ^. TestId)
            `innerJoin` table @Skill `on` (\(_ :& q :& _ :& s) -> q ^. StemSkill ==. s ^. SkillId)
        where_ $ x ^. OptionId ==. val oid
        return (x, q ^. StemText, e ^. TestName, s ^. SkillName))

    (fw0,et0) <- generateFormPost formOptionDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAnswerOption
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "options/option") 


formOptionDelete :: Form ()
formOptionDelete extra = return (pure (), [whamlet|^{extra}|])


postOptionsR :: TestId -> StemId -> Handler Html
postOptionsR eid qid = do
    ((fr,widget),enctype) <- runFormPost $ formOption eid qid Nothing
    case fr of
      FormSuccess x -> do
          runDB $ insert_ x
          addMessageI msgSuccess MsgNewRecordAdded
          redirect $ DataR $ OptionsR eid qid
          
      _ -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgOption
          $(widgetFile "options/create")


getOptionsR :: TestId -> StemId -> Handler Html
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


getOptionCreateFormR :: TestId -> StemId -> Handler Html
getOptionCreateFormR eid qid = do
    (widget,enctype) <- generateFormPost $ formOption eid qid Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgOption
        $(widgetFile "options/create")


formOption :: TestId -> StemId -> Maybe (Entity Option) -> Form Option
formOption _ qid option extra = do
    (ordinalR,ordinalV) <- mreq uniqueOrdinalField FieldSettings
        { fsLabel = SomeMessage MsgOrdinal
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (optionOrdinal . entityVal <$> option)
        
    (textR,textV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgText
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (optionText . entityVal <$> option)
        
    (keyR,keyV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgCorrectAnswer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (optionKey . entityVal <$> option)
        
    (pointsR,pointsV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgAnswerPoints
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (optionPoints . entityVal <$> option)

    let r = Option qid <$> ordinalR <*> textR <*> keyR <*> pointsR
    let w = [whamlet|
                    #{extra}

                    ^{md3widget ordinalV}

                    ^{md3textareaWidget textV}

                    ^{md3switchWidget keyV}

                    ^{md3widget pointsV}
                    |]
    return (r,w)

  where

      uniqueOrdinalField = checkM uniqueOrdinal textField

      uniqueOrdinal :: Text -> Handler (Either AppMessage Text)
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
