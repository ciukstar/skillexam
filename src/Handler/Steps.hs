{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Steps
  ( getStepR
  , postStepR
  , postCompleteR
  , getSummaryR
  , postTerminateR
  , getRemainingTimeR
  ) where


import ClassyPrelude (readMay)

import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Aeson (object, (.=))
import Data.Complex (Complex ((:+)))
import qualified Data.List.Safe as LS
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack, Text)
import Data.Text.ICU.Calendar
    ( calendar, CalendarType (TraditionalCalendarType), setDay, setHour
    , setMinute, setSecond
    )
import Data.Text.ICU.Types (LocaleName(Locale))
import Data.Text.ICU.DateFormatter
    ( standardDateFormatter, FormatStyle (ShortFormatStyle, DefaultFormatStyle)
    , formatCalendar
    )
import Data.Time.Clock
    (getCurrentTime, UTCTime (utctDayTime, UTCTime), secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeToTimeOfDay)

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, where_, val, min_, max_
    , (^.), (==.), (>.), (<.), (=.), (:&) ((:&))
    , Value (unValue, Value), orderBy, asc, just, selectQuery, subSelectMaybe
    , delete, update, set, innerJoin, on, sum_, coalesceDefault
    )
import Database.Persist
    ( Entity (Entity), entityVal, insertMany_
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App, Form, Handler
    , Route (HomeR, StepR, CompleteR, SummaryR, TerminateR, RemainingTimeR)
    , AppMessage
      ( MsgQuestion, MsgPrevious, MsgNext, MsgComplete, MsgFinish
      , MsgExam, MsgSummary, MsgCandidate, MsgAttempt, MsgCancel
      , MsgStop, MsgTimeStart, MsgTimeEnd, MsgStopThisExam, MsgPleaseConfirm
      , MsgOfTotal, MsgCode, MsgInvalidData, MsgTimeRemaining, MsgExamResults
      , MsgPass, MsgFail, MsgStatus, MsgPassMark, MsgScore, MsgExamResults
      , MsgExamInfo, MsgInvalidFormData, MsgExamSuccessfullyCancelled
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
    ( ExamId, Exam
    , StemId, Stem (Stem, stemOrdinal), StemType (MultiResponse)
    , TestId, Test
    , OptionId, Option (Option)
    , Answer (Answer)
    , Candidate (Candidate, candidateFamilyName, candidateGivenName, candidateAdditionalName)
    , EntityField
      ( StemId, StemTest, StemOrdinal, OptionStem, OptionOrdinal
      , AnswerExam, AnswerStem, AnswerOption, ExamEnd
      , ExamId, TestId, CandidateId, ExamTest, ExamStart, TestDuration
      , OptionKey, OptionPoints, ExamAttempt, TestPass, TestCode
      , TestName, OptionId, ExamCandidate
      ), msgSuccess
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (Lang)
import qualified Text.Printf as Printf

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI
    , MonadHandler (liftHandler)
    , redirectUltDest, preEscapedToMarkup, lookupPostParams
    , TypedContent (TypedContent), selectRep, provideRep
    , provideJson, invalidArgsI, ContentType
    , ToContent (toContent), Content, ToTypedContent (toTypedContent)
    , HasContentType (getContentType)
    , RenderMessage (renderMessage)
    , getYesod, languages, addHeader, newIdent, getMessageRender, addMessageI
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Core.Handler (redirect)
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Fields (Textarea (unTextarea), urlField, textField)
import Yesod.Form.Input (ireq, runInputPost, runInputGet, iopt)
import Yesod.Form.Types ( FormResult (FormSuccess, FormFailure) )
import Yesod.Persist.Core (YesodPersist(runDB))


getRemainingTimeR :: ExamId -> Handler TypedContent
getRemainingTimeR eid = do
    now <- liftIO getCurrentTime
    
    mx <- runDB ( selectOne ( do
        e :& t <- from $ table @Exam
            `innerJoin` table @Test `on` (\(e :& t) -> e ^. ExamTest ==. t ^. TestId)
        where_ $ e ^. ExamId ==. val eid
        return (e ^. ExamStart, t ^. TestDuration) ) )
        
    case mx of
      Just (Value s, Value l) -> selectRep $ provideJson $ object
          [ "left" .= (utctDayTime s + secondsToDiffTime (round (l * 60)) - utctDayTime now)
          , "total" .= (round (l * 60) :: Integer)
          ]
          
      _otherwise -> invalidArgsI [MsgInvalidData]


postTerminateR :: TestId -> ExamId -> Handler Html
postTerminateR tid eid = do
    runDB $ delete $ do
        x <- from $ table @Exam
        where_ $ x ^. ExamId ==. val eid
        where_ $ x ^. ExamTest ==. val tid

    addMessageI msgSuccess MsgExamSuccessfullyCancelled
    redirectUltDest HomeR


getSummaryR :: TestId -> ExamId -> Handler TypedContent
getSummaryR tid eid = do

    tz <- fromMaybe "GMT+0000" <$> runInputGet (iopt textField "tz")
    
    let unv (Value code,Value name,Value attempt,Value start,Value end,Value score,Value pass) =
            (code,name,attempt,start,end,score,pass)

    res <- (unv <$>) <$> runDB ( selectOne $ do
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
        return (code,name,attempt,start,end,score,pass) )

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
            
            loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
            cal <- liftIO $ calendar "UTC" loc TraditionalCalendarType
            fmt <- liftIO $ standardDateFormatter DefaultFormatStyle ShortFormatStyle loc tz
            
            let tfmt :: UTCTime -> Text
                tfmt (UTCTime day time) = case timeToTimeOfDay time of
                  TimeOfDay h m ps -> formatCalendar fmt
                    (setSecond (setMinute (setHour (setDay cal day) h) m)
                      (round (ps * 1e-12)))

            
            case (font, fontb,candidate,res) of
              (Right f, Right fb, Just (Entity _ c), Just result) -> do
                  let name = renderMessage app langs MsgExamResults
                  addHeader "Content-Disposition"
                      ("inline; name=" <> name <> "; filename=" <> name <> ".pdf")
                  return $ pdf app langs tfmt c result (PDFRect 0 0 612 792) f fb
                  
              _otherwise -> invalidArgsI [MsgInvalidData]


        provideRep $ do
          defaultLayout $ do
              setTitleI MsgFinish
              $(widgetFile "steps/summary")


pdf :: App -> [Lang] -> (UTCTime -> Text) -> Candidate
    -> (Text,Text,Int,UTCTime,Maybe UTCTime,Double,Double)
    -> PDFRect -> AnyFont -> AnyFont -> PDF ()
pdf app langs tfmt candidate (code,name,attempt,start,end,score,pass) (PDFRect x _ x' y') fontr fontb = do

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
            txt $ tfmt start

    -- row
    let (_,d80) = drawTextBox x9 (y9 - 10) cw0 30 NE NormalParagraph (Font bfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ renderMessage app langs MsgTimeEnd

    let (_,d81) = drawTextBox col1x (y9 - 10) cw1 30 NE NormalParagraph (Font rfont black black) $ do
          setJustification LeftJustification
          paragraph $ do
            txt $ maybe "" tfmt end

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



postCompleteR :: TestId -> ExamId -> StemId -> Handler Html
postCompleteR tid eid qid = do
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
          redirect $ SummaryR tid eid
          
      _otherwise -> defaultLayout $ do
          setTitleI MsgQuestion
          idTimer <- newIdent
          idProgressTimer <- newIdent
          idDialogTerminate <- newIdent
          idFormOptions <- newIdent
          $(widgetFile "steps/step")


postStepR :: TestId -> ExamId -> StemId -> Handler Html
postStepR tid eid qid = do

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
          
      _otherwise -> defaultLayout $ do
          setTitleI MsgQuestion
          idTimer <- newIdent
          idProgressTimer <- newIdent
          idDialogTerminate <- newIdent
          idFormOptions <- newIdent
          $(widgetFile "steps/step")


getStepR :: TestId -> ExamId -> StemId -> Handler Html
getStepR tid eid qid = do
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

    (fw,et) <- generateFormPost $ formOptions eid qid options

    defaultLayout $ do
        setTitleI MsgQuestion
        idTimer <- newIdent
        idProgressTimer <- newIdent
        idDialogTerminate <- newIdent
        idFormOptions <- newIdent
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
    
    maybeAnswers <- mapM ((toSqlKey <$>) . readMay) <$> lookupPostParams answer

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
