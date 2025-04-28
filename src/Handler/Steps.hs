{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Steps
  ( getStepR, postStepR
  , getStepInvalidR
  , postCompleteR
  , getSummaryR
  , postCancelR
  , getRemainingTimeR
  , getWebSocketTimeoutR
  ) where

import ClassyPrelude as CP (readMay)

import Control.Concurrent.STM.TVar (readTVar)
import Control.Concurrent.STM (atomically, readTChan, dupTChan)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Aeson (object, (.=))
import Data.Complex (Complex ((:+)))
import qualified Data.Map as M (lookup)
import Data.Text (pack, Text)
import Data.Time.Clock
    ( getCurrentTime, UTCTime (utctDayTime), secondsToDiffTime
    )
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, where_, val, min_, max_
    , (^.), (==.), (>.), (<.), (=.), (:&) ((:&))
    , Value (unValue, Value), orderBy, asc, just, selectQuery
    , delete, update, set, innerJoin, on, sum_, coalesceDefault
    , subSelectMaybe
    )
import Database.Persist
    ( Entity (Entity), entityVal, insertMany_
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App (exams), Form, Handler, widgetSnackbar
    , Route
      ( HomeR, StepR, CompleteR, SummaryR, CancelR, RemainingTimeR
      , WebSocketTimeoutR, StepInvalidR
      )
    , AppMessage
      ( MsgQuestion, MsgPrevious, MsgNext, MsgComplete, MsgFinish
      , MsgExam, MsgSummary, MsgCandidate, MsgAttempt, MsgCancel, MsgTimeIsUp
      , MsgStop, MsgTimeStart, MsgTimeEnd, MsgStopThisExam, MsgPleaseConfirm
      , MsgOfTotal, MsgCode, MsgInvalidData, MsgTimeRemaining, MsgExamResults
      , MsgPass, MsgFail, MsgStatus, MsgPassMark, MsgScore, MsgExamResults
      , MsgInvalidFormData, MsgExamSuccessfullyCancelled, MsgExamTimeHasExpired
      , MsgExamInfo, MsgAllottedExamTimeHasExpired, MsgExamWasCancelled, MsgHome
      , MsgClose, MsgInvalidExam, MsgExamCompleted
      )
    )

import Graphics.PDF.Typesetting
    ( drawTextBox, paragraph, setJustification, txt, Orientation(NE)
    , Justification(LeftJustification)
    , StandardParagraphStyle(NormalParagraph), StandardStyle(Font)
    )
import Graphics.PDF
    ( PDF, addPage, strokeColor, drawWithPage, newSection, standardViewerPrefs
    , pdfByteString, black, standardDocInfo
    , Rectangle (Rectangle)
    , Shape (stroke)    
    , PDFDocumentInfo (author, subject, compressed, viewerPreferences)
    , PDFRect (PDFRect)    
    , PDFFont (PDFFont), mkStdFont
    , FontName (Times_Roman, Times_Bold)
    , AnyFont, Line (Line), Color (Rgb)
    , PDFViewerPreferences (displayDoctitle)
    )

import Model
    ( msgSuccess, ExamId, Exam (Exam)
    , StemId, Stem (Stem, stemOrdinal), StemType (MultiResponse)
    , TestId, Test
    , OptionId, Option (Option)
    , Answer (Answer)
    , ExamStatus
      ( ExamStatusTimeout, ExamStatusCanceled, ExamStatusOngoing
      , ExamStatusCompleted
      )
    , CandidateId
    , Candidate
      ( Candidate, candidateGivenName, candidateAdditionalName
      , candidateFamilyName
      )
    , TimeUnit (TimeUnitMinute, TimeUnitHour)
    , Tokens
    , EntityField
      ( StemId, StemTest, StemOrdinal, OptionStem, OptionOrdinal
      , AnswerExam, AnswerStem, AnswerOption, ExamEnd, TestCode
      , ExamId, TestId, CandidateId, ExamTest, ExamStart, TestDuration
      , OptionKey, OptionPoints, ExamAttempt, TestPass, ExamCandidate
      , TestName, OptionId, ExamStatus, TestDurationUnit
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (Lang)
import qualified Text.Printf as Printf

import Yesod.Core
    ( ContentType, Yesod(defaultLayout), MonadHandler (liftHandler)
    , redirectUltDest, preEscapedToMarkup, lookupPostParams, setTitleI
    , TypedContent (TypedContent), selectRep, provideRep, provideJson
    , ToContent (toContent), Content, ToTypedContent (toTypedContent)
    , HasContentType (getContentType), RenderMessage (renderMessage)
    , getYesod, languages, addHeader, newIdent, getMessageRender
    , addMessageI, getMessages, invalidArgsI
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Core.Handler (redirect)
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Fields (Textarea (unTextarea), urlField)
import Yesod.Form.Input (ireq, runInputPost)
import Yesod.Form.Types ( FormResult (FormSuccess, FormFailure) )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.WebSockets ( sendTextData, webSockets)


getWebSocketTimeoutR :: ExamId -> Handler ()
getWebSocketTimeoutR eid = webSockets $ do
    app <- getYesod
    chan <- liftIO $ atomically $ do
        m <- readTVar (exams app)
        case M.lookup eid m of
          Nothing -> return Nothing
          Just chan -> Just <$> dupTChan chan

    case chan of
      Nothing -> return ()
      Just c -> do
          let loop = do
                  status <- liftIO $ atomically $ readTChan c
                  case status of
                    ExamStatusTimeout -> do
                        now <- liftIO getCurrentTime
                        liftHandler $ runDB $ update $ \x -> do
                            set x [ ExamStatus =. val ExamStatusTimeout
                                  , ExamEnd =. just (val now)
                                  ]
                            where_ $ x ^. ExamId ==. val eid
                            where_ $ x ^. ExamStatus ==. val ExamStatusOngoing
                        sendTextData signalTimeout
                    _otherwise -> return ()
          loop
          
signalTimeout :: Text
signalTimeout = "SIGNAL_TIMEOUT"


getRemainingTimeR :: ExamId -> Handler TypedContent
getRemainingTimeR eid = do
    now <- liftIO getCurrentTime
    
    mx <- runDB ( selectOne ( do
        e :& t <- from $ table @Exam
            `innerJoin` table @Test `on` (\(e :& t) -> e ^. ExamTest ==. t ^. TestId)
        where_ $ e ^. ExamId ==. val eid
        return (e ^. ExamStart, t ^. TestDuration, t ^. TestDurationUnit) ) )
        
    case mx of
      Just (Value s, Value l, Value u) -> selectRep $ provideJson $ object
          [ "left" .= (utctDayTime s + secondsToDiffTime (round (l * toSeconds u)) - utctDayTime now)
          , "total" .= (round (l * toSeconds u) :: Integer)
          ]
          
      _otherwise -> invalidArgsI [MsgInvalidData]

  where
      toSeconds unit = case unit of
        TimeUnitMinute -> 60
        TimeUnitHour -> 3600


postCancelR :: CandidateId -> ExamId -> Tokens -> Handler Html
postCancelR _cid eid _tokens = do
    now <- liftIO getCurrentTime
    runDB $ update $ \x -> do
        set x [ ExamStatus =. val ExamStatusCanceled
              , ExamEnd =. just (val now)
              ]
        where_ $ x ^. ExamId ==. val eid

    addMessageI msgSuccess MsgExamSuccessfullyCancelled
    redirectUltDest HomeR


getSummaryR :: CandidateId -> TestId -> ExamId -> Tokens -> Handler TypedContent
getSummaryR cid tid eid tokens = do
    
    let unv (Value code,Value name,Value attempt,Value start,Value end,Value score,Value pass) =
            (code,name,attempt,start,end,score,pass)

    res <- ((unv <$>) <$>) $ runDB $ selectOne $ do
        (_,attempt,start,end,code,name,pass) :& (_,score) <- from ( selectQuery ( do
                x :& t <- from $ table @Exam
                    `innerJoin` table @Test `on` (\(x :& t) -> x ^. ExamTest ==. t ^. TestId)
                where_ $ x ^. ExamId ==. val eid
                return (x ^. ExamId,x ^. ExamAttempt,x ^. ExamStart,x ^. ExamEnd,t ^. TestCode,t ^. TestName,t ^. TestPass)
            ) `innerJoin` selectQuery ( do
                _ :& o :& e <- from $ table @Answer
                    `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                    `innerJoin` table @Exam `on` (\(a :& _ :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                where_ $ e ^. ExamId ==. val eid
                where_ $ o ^. OptionKey
                return (e ^. ExamId, coalesceDefault [sum_ (o ^. OptionPoints)] (val (0 :: Double)))
                ) `on` (\((eid',_,_,_,_,_,_) :& (eid'',_)) -> eid' ==. eid'') )
        return (code,name,attempt,start,end,score,pass)

    candidate <- runDB $ selectOne $ do
        x :& c <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
        where_ $ x ^. ExamId ==. val eid
        return c

    selectRep $ do
        provideRep $ do
            app <- getYesod
            langs <- languages
            font <- liftIO $ mkStdFont Times_Roman
            fontb <- liftIO $ mkStdFont Times_Bold
            
            case (font, fontb,candidate,res) of
              (Right f, Right fb, Just (Entity _ c), Just result) -> do
                  let name = renderMessage app langs MsgExamResults
                  addHeader "Content-Disposition"
                      ("inline; name=" <> name <> "; filename=" <> name <> ".pdf")
                  return $ pdf app langs c result (PDFRect 0 0 612 792) f fb
                  
              _otherwise -> invalidArgsI [MsgInvalidData]


        provideRep $ do
          defaultLayout $ do
              setTitleI MsgSummary
              $(widgetFile "steps/summary")


pdf :: App -> [Lang] -> Candidate
    -> (Text,Text,Int,UTCTime,Maybe UTCTime,Double,Double)
    -> PDFRect -> AnyFont -> AnyFont -> PDF ()
pdf app langs candidate (code,name,attempt,start,end,score,pass) (PDFRect x _ x' y') fontr fontb = do

    let hfont = PDFFont fontb 18
    let rfont = PDFFont fontr 16
    let bfont = PDFFont fontb 16

    let (mt, mr, _, ml) = (72,72,72,72) :: (Double,Double,Double,Double)
    let cw0 = 0.3 * (x' - x - ml - mr)
    let cw1 = 0.7 * (x' - x - ml - mr)

    let (col0x,col0y) = (x + ml, y' - mt)
    let (col1x,_) = (col0x + cw0, y' - mt)

    -- header 0
    let (Rectangle (x0 :+ y0) _,hd00) = drawTextBox col0x col0y (cw0 + cw1) 30 NE NormalParagraph (Font hfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgExamResults

    -- row 0
    let (Rectangle (x1 :+ y1) _,d00) = drawTextBox x0 (y0 - 30) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgStatus

    let (_,d01) = drawTextBox col1x (y0 - 30) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ if score >= pass then renderMessage app langs MsgPass else renderMessage app langs MsgFail

    -- row 1
    let (Rectangle (x2 :+ y2) _,d10) = drawTextBox x1 (y1 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgScore

    let (_,d11) = drawTextBox col1x (y1 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ pack (printf "%.0f" score)

    -- row 2
    let (Rectangle (x3 :+ y3) _,d20) = drawTextBox x2 (y2 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgPassMark

    let (_,d21) = drawTextBox col1x (y2 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ pack (printf "%.0f" pass)

    -- header 1
    let (Rectangle (x4 :+ y4) _,hd01) = drawTextBox x3 (y3 - 72) (cw0 + cw1) 30 NE NormalParagraph (Font hfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgExamInfo

    -- row
    let (Rectangle (x5 :+ y5) _,d30) = drawTextBox x4 (y4 - 30) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgExam

    let (_,d31) = drawTextBox col1x (y4 - 30) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt name

    -- row
    let (Rectangle (x6 :+ y6) _,d40) = drawTextBox x5 (y5 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgCode

    let (_,d41) = drawTextBox col1x (y5 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt code

    -- row
    let (Rectangle (x7 :+ y7) _,d50) = drawTextBox x6 (y6 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgCandidate

    let (_,d51) = drawTextBox col1x (y6 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ candidateFamilyName candidate
                <> " " <> candidateGivenName candidate
                <> maybe "" (" " <>) (candidateAdditionalName candidate)

    -- row
    let (Rectangle (x8 :+ y8) _,d60) = drawTextBox x7 (y7 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgAttempt

    let (_,d61) = drawTextBox col1x (y7 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ pack (show attempt)

    -- row
    let (Rectangle (x9 :+ y9) _,d70) = drawTextBox x8 (y8 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgTimeStart

    let (_,d71) = drawTextBox col1x (y8 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ (pack . show) start

    -- row
    let (_,d80) = drawTextBox x9 (y9 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgTimeEnd

    let (_,d81) = drawTextBox col1x (y9 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ maybe "" (pack . show) end

    page <- addPage Nothing
    newSection (renderMessage app langs MsgExamResults) Nothing Nothing $ do
        drawWithPage page $ do
            hd00
            strokeColor $ Rgb 0 0 0
            stroke $ Line x0 y0 (x0 + cw0 + cw1) y0
            d00 >> d01
            d10 >> d11
            d20 >> d21
            hd01
            stroke $ Line x4 y4 (x4 + cw0 + cw1) y4
            d30 >> d31
            d40 >> d41
            d50 >> d51
            d60 >> d61
            d70 >> d71
            d80 >> d81


mimePdf :: ContentType
mimePdf = "application/pdf"


instance ToContent (PDF ()) where
    toContent :: PDF () -> Content
    toContent = toContent . pdfByteString
        standardDocInfo { author = "Starciuc Sergiu"
                        , subject = "Exam Results" 
                        , compressed = False
                        , viewerPreferences = standardViewerPrefs { displayDoctitle = True }
                        }
        (PDFRect 0 0 612 792)


instance ToTypedContent (PDF ()) where
    toTypedContent :: PDF () -> TypedContent
    toTypedContent = TypedContent mimePdf . toContent


instance HasContentType (PDF ()) where
    getContentType :: Monad m => m (PDF ()) -> ContentType
    getContentType _ = mimePdf


postCompleteR :: CandidateId -> TestId -> ExamId -> StemId -> Tokens -> Handler Html
postCompleteR cid tid eid qid tokens = do
    
    cnt <- ((unValue =<<) <$>) $ runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemTest ==. val tid
        return $ max_ $ x ^. StemOrdinal
        
    stem <- runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemId ==. val qid
        return x
        
    prev <- ((unValue <$>) <$>) $ runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemTest ==. val tid
        where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
            ( from $ selectQuery $ do
                  y <- from $ table @Stem
                  where_ $ y ^. StemTest ==. x ^. StemTest
                  where_ $ just (y ^. StemOrdinal) <. val (stemOrdinal . entityVal <$> stem)
                  return $ max_ $ y ^. StemOrdinal
            )
        return $ x ^. StemId
        
    next <- ((unValue <$>) <$>) $ runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemTest ==. val tid
        where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
            ( from $ selectQuery $ do
                  y <- from $ table @Stem
                  where_ $ y ^. StemTest ==. x ^. StemTest
                  where_ $ just (y ^. StemOrdinal) >. val (stemOrdinal . entityVal <$> stem)
                  return $ min_ $ y ^. StemOrdinal
            )
        return $ x ^. StemId

    options <- runDB $ select $ do
        x <- from $ table @Option
        where_ $ x ^. OptionStem ==. val qid
        orderBy [asc (x ^. OptionOrdinal)]
        return x
        
    ((fr,fw),et) <- runFormPost $ formOptions eid qid options
    case fr of
      FormSuccess rs -> do
          runDB $ do
              delete $ do
                  x <- from $ table @Answer
                  where_ $ x ^. AnswerExam ==. val eid
                  where_ $ x ^. AnswerStem ==. val qid

              now <- liftIO getCurrentTime
              insertMany_ ((\(AnswerData e q o) -> Answer e q o now) <$> rs)

              update $ \x -> do
                  set x [ExamEnd =. just (val now)]
                  where_ $ x ^. ExamId ==. val eid
          redirect $ SummaryR cid tid eid tokens
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgQuestion
              idLabelTimer <- newIdent
              idTimer <- newIdent
              idProgressTimer <- newIdent
              idDialogTerminate <- newIdent
              idFormOptions <- newIdent
              idOverlay <- newIdent
              idDialogTimeout <- newIdent
              $(widgetFile "steps/step")


getStepInvalidR :: CandidateId -> TestId -> ExamId -> Tokens -> Handler Html
getStepInvalidR cid tid eid tokens = do
    
    exam <- runDB $ selectOne $ do
        x <- from $ table @Exam
        where_ $ x ^. ExamId ==. val eid
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgInvalidExam
        $(widgetFile "steps/invalid")


postStepR :: CandidateId -> TestId -> ExamId -> StemId -> Tokens -> Handler Html
postStepR cid tid eid qid tokens = do
    
    exam <- runDB $ selectOne $ do
        x <- from $ table @Exam
        where_ $ x ^. ExamId ==. val eid
        where_ $ x ^. ExamStatus ==. val ExamStatusOngoing
        return x

    case exam of
      Nothing -> redirect $ StepInvalidR cid tid eid tokens
      Just _ -> do
          location <- runInputPost $ ireq urlField "location"

          cnt <- (unValue =<<) <$> runDB ( selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              return $ max_ $ x ^. StemOrdinal )

          stem <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemId ==. val qid
              return x

          prev <- (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
                  ( from $ selectQuery $ do
                        y <- from $ table @Stem
                        where_ $ y ^. StemTest ==. x ^. StemTest
                        where_ $ just (y ^. StemOrdinal) <. val (stemOrdinal . entityVal <$> stem)
                        return $ max_ $ y ^. StemOrdinal
                  )
              return $ x ^. StemId )

          next <- (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
                  ( from $ selectQuery $ do
                        y <- from $ table @Stem
                        where_ $ y ^. StemTest ==. x ^. StemTest
                        where_ $ just (y ^. StemOrdinal) >. val (stemOrdinal . entityVal <$> stem)
                        return $ min_ $ y ^. StemOrdinal
                  )
              return $ x ^. StemId )

          options <- runDB $ select $ do
              x <- from $ table @Option
              where_ $ x ^. OptionStem ==. val qid
              orderBy [asc (x ^. OptionOrdinal)]
              return x

          ((fr,fw),et) <- runFormPost $ formOptions eid qid options
          case fr of
            FormSuccess rs -> do
                runDB $ delete $ do
                    x <- from $ table @Answer
                    where_ $ x ^. AnswerExam ==. val eid
                    where_ $ x ^. AnswerStem ==. val qid

                now <- liftIO getCurrentTime
                runDB $ insertMany_ ((\(AnswerData e q o) -> Answer e q o now) <$> rs)

                redirect location

            _otherwise -> do
                msgr <- getMessageRender
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgQuestion
                    idLabelTimer <- newIdent
                    idTimer <- newIdent
                    idProgressTimer <- newIdent
                    idDialogTerminate <- newIdent
                    idFormOptions <- newIdent
                    idOverlay <- newIdent
                    idDialogTimeout <- newIdent
                    $(widgetFile "steps/step")


getStepR :: CandidateId -> TestId -> ExamId -> StemId -> Tokens -> Handler Html
getStepR cid tid eid qid tokens = do
    
    exam <- runDB $ selectOne $ do
        x <- from $ table @Exam
        where_ $ x ^. ExamId ==. val eid
        where_ $ x ^. ExamStatus ==. val ExamStatusOngoing
        return x

    case exam of
      Nothing -> redirect $ StepInvalidR cid tid eid tokens
      Just _ -> do
          cnt <- ((unValue =<<) <$>) $ runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              return $ max_ $ x ^. StemOrdinal

          stem <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemId ==. val qid
              return x

          prev <- ((unValue <$>) <$>) $ runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
                  ( from $ selectQuery $ do
                        y <- from $ table @Stem
                        where_ $ y ^. StemTest ==. x ^. StemTest
                        where_ $ just (y ^. StemOrdinal) <. val (stemOrdinal . entityVal <$> stem)
                        return $ max_ $ y ^. StemOrdinal
                  )
              return $ x ^. StemId

          next <- ((unValue <$>) <$>) $ runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe
                  ( from $ selectQuery $ do
                        y <- from $ table @Stem
                        where_ $ y ^. StemTest ==. x ^. StemTest
                        where_ $ just (y ^. StemOrdinal) >. val (stemOrdinal . entityVal <$> stem)
                        return $ min_ $ y ^. StemOrdinal
                  )
              return $ x ^. StemId

          options <- runDB $ select $ do
              x <- from $ table @Option
              where_ $ x ^. OptionStem ==. val qid
              orderBy [asc (x ^. OptionOrdinal)]
              return x

          (fw,et) <- generateFormPost $ formOptions eid qid options

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgQuestion
              idLabelTimer <- newIdent
              idTimer <- newIdent
              idProgressTimer <- newIdent
              idDialogTerminate <- newIdent
              idFormOptions <- newIdent
              idOverlay <- newIdent
              idDialogTimeout <- newIdent
              $(widgetFile "steps/step")


data AnswerData = AnswerData !ExamId !StemId !OptionId


formOptions :: ExamId -> StemId -> [Entity Option] -> Form [AnswerData]
formOptions eid qid options extra = do

    stem <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Stem
        where_ $ x ^. StemId ==. val qid
        return x

    answers <- (unValue <$>) <$> liftHandler ( runDB $ select $ do
        x <- from $ table @Answer
        where_ $ x ^. AnswerExam ==. val eid
        where_ $ x ^. AnswerStem ==. val qid
        return $ x ^. AnswerOption )

    msgr <- getMessageRender
    let answer = "answer"
    
    maybeAnswers <- mapM ((toSqlKey <$>) . CP.readMay) <$> lookupPostParams answer

    let r = case maybeAnswers of
              Nothing -> FormFailure [msgr MsgInvalidFormData]
              Just ass -> FormSuccess (AnswerData eid qid <$> ass)

    let w = [whamlet|
                    #{extra}

                    $maybe Entity _ (Stem _ _ _ _ stype _) <- stem
                      $if MultiResponse == stype
                        $forall Entity oid (Option _ i text _ _) <- options
                          $with ident <- fromSqlKey oid
                            <div.row>
                              <span>#{i}
                              <label.checkbox.max>
                                <input type=checkbox name=#{answer} value=#{ident} :elem oid answers:checked>
                                <span>
                                  #{preEscapedToMarkup $ unTextarea text}

                      $else
                        $forall Entity oid (Option _ i text _ _) <- options
                          $with ident <- fromSqlKey oid
                            <div.row>
                              <span>#{i}
                              <label.radio.max>
                                <input type=radio name=#{answer} value=#{ident} :elem oid answers:checked>
                                <span>
                                  #{preEscapedToMarkup $ unTextarea text}
                    |]
    return (r,w)


printf :: String -> Double -> String
printf = Printf.printf
