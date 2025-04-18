{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Exams
  ( getExamsR, postExamsR
  , getExamR
  , getExamsLoginR
  , getExamsAfterLoginR
  , getExamEnrollmentFormR, postExamEnrollmentFormR
  , getExamUserEnrollmentR
  , getSearchExamsR
  , getSearchExamR
  ) where


import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(bimap, second))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( SqlQuery, SqlExpr, Value (Value, unValue), selectOne, from, table
    , (^.), (==.), (:&) ((:&)), (%), (++.)
    , where_, val, select, orderBy, innerJoin, sum_, desc, on, in_, like
    , subSelectList, selectQuery, upper_, groupBy, coalesceDefault, just
    , asc, subSelectMaybe, min_, max_, leftJoin, countRows
    )
import Database.Persist (Entity (Entity), entityVal, insert)

import Foundation
  ( App, Handler, Form, widgetAccount, widgetMainMenu, widgetSnackbar
  , Route
    ( DataR, ExamsR, ExamR, PhotoPlaceholderR, ExamsAfterLoginR
    , SearchExamsR, AuthR, HomeR, ExamEnrollmentFormR, StepR
    , ExamUserEnrollmentR, StaticR, ExamsLoginR, SearchExamR
    )
  , DataR (CandidatePhotoR)
  , AppMessage
    ( MsgTakeNewExam, MsgMyExams, MsgNoExams, MsgSelect, MsgTests, MsgClose
    , MsgLogin, MsgPhoto, MsgAttempt, MsgExam, MsgBack, MsgTakePhoto
    , MsgExamResults, MsgStatus, MsgPass, MsgFail, MsgScore, MsgPassScore
    , MsgMaxScore, MsgCandidate, MsgCompleted, MsgSearch, MsgCancel
    , MsgAuthenticate, MsgLoginToSeeYourExamsPlease, MsgEnrollment
    , MsgStartExam, MsgSelectATestForTheExam, MsgInvalidFormData, MsgBirthday
    , MsgNoQuestionsForTheTest, MsgPoints, MsgNumberOfQuestions, MsgName
    , MsgMinutes, MsgDuration, MsgDescr, MsgCode, MsgFamilyName, MsgSave
    , MsgGivenName, MsgAdditionalName, MsgUploadPhoto, MsgRetakeThisExam
    , MsgLoginRequired, MsgNoExamsWereFoundForSearchTerms, MsgOngoing
    , MsgTimeout, MsgTimeCompleted, MsgHours
    )
  )

import Material3 (md3radioField, md3widget)

import Model
    ( msgError
    , CandidateId
    , Candidate
      ( Candidate, candidateAdditionalName, candidateGivenName, candidateBday
      , candidateFamilyName
      )
    , ExamId, Exam (Exam, examEnd, examStatus)
    , ExamStatus (ExamStatusOngoing, ExamStatusCompleted, ExamStatusTimeout)
    , TestId, Test (Test, testPass, testName)
    , Option, Stem, Answer, UserId
    , TimeUnit (TimeUnitMinute, TimeUnitHour)
    , EntityField
      ( CandidateId, ExamCandidate, ExamStart, ExamAttempt, ExamTest, TestId
      , ExamId, StemId, StemTest, OptionStem, OptionPoints, AnswerOption
      , AnswerExam, TestName, CandidateUser, OptionId, StemOrdinal
      )
    )

import Settings ( widgetFile )
import Settings.StaticFiles
    ( img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Cassius (cassius)
import qualified Text.Printf as Printf (printf)

import Yesod.Auth (Route(LoginR), YesodAuth (maybeAuthId))
import Yesod.Core
    ( Html, Yesod (defaultLayout), setTitleI, getMessages, whamlet, redirect
    , SomeMessage (SomeMessage), MonadHandler (liftHandler), ToWidget (toWidget)
    , addMessageI, FileInfo, setUltDest, YesodRequest (reqGetParams), getRequest
    )
import Yesod.Core.Handler
    ( HandlerFor, setUltDestCurrent, getUrlRender, newIdent
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( searchField, textField, urlField, optionsPairs, fileField
    , dayField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FieldView (fvInput, fvId, fvErrors), FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postExamEnrollmentFormR :: UserId -> CandidateId -> TestId -> Handler Html
postExamEnrollmentFormR uid cid tid = do
    
    ((fr,_),_) <- runFormPost $ formEnrollment cid tid
    case fr of
      FormSuccess (cid',tid', attempt) -> do
          now <- liftIO getCurrentTime
          eid <- runDB $ insert (Exam tid' cid' ExamStatusOngoing attempt now Nothing)
          stem <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe ( from $ selectQuery $ do
                  y <- from $ table @Stem
                  where_ $ y ^. StemTest ==. x ^. StemTest
                  return $ min_ $ y ^. StemOrdinal )
              return x
          case stem of
            Just (Entity qid _) -> do
                setUltDest $ ExamsR uid
                redirect $ StepR uid tid eid qid
            
            Nothing -> do
                addMessageI msgError MsgNoQuestionsForTheTest
                redirect $ ExamEnrollmentFormR uid cid tid
      
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ ExamEnrollmentFormR uid cid tid


getExamEnrollmentFormR :: UserId -> CandidateId -> TestId -> Handler Html
getExamEnrollmentFormR uid cid tid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
        
    info <- (second (((+1) <$>) <$>) <$>) <$> runDB ( selectOne $ do
          (x :& (_,nq,maxScore) :& (_,nt)) <- from $ table @Test
            `leftJoin` from ( selectQuery $ do
                                   q :& (_,maxScore) <- from $ table @Stem
                                       `leftJoin` from ( selectQuery $ do
                                                            o <- from $ table @Option
                                                            groupBy (o ^. OptionStem)
                                                            return ( o ^. OptionStem
                                                                   , sum_ (o ^. OptionPoints) :: SqlExpr (Value (Maybe Double))
                                                                   )
                                                        ) `on` (\(q :& (qid,_)) -> just (q ^. StemId) ==. qid)
                                   groupBy (q ^. StemTest)
                                   return ( q ^. StemTest
                                          , countRows :: SqlExpr (Value Int)
                                          , coalesceDefault [sum_ maxScore] (val 0) :: SqlExpr (Value Double)
                                          )
                             ) `on` (\(x :& (xid,_,_)) -> just (x ^. TestId) ==. xid)
            
            `leftJoin` from ( selectQuery $ do
                                   r <- from $ table @Exam
                                   where_ $ r ^. ExamCandidate ==. val cid
                                   groupBy (r ^. ExamTest)
                                   return ( r ^. ExamTest
                                          , countRows :: SqlExpr (Value Int)
                                          )
                             ) `on` (\(x :& _ :& (xid,_)) -> just (x ^. TestId) ==. xid)
              
          where_ $ x ^. TestId ==. val tid
          return (((x,nq),maxScore),nt) )
    
    (fw,et) <- generateFormPost $ formEnrollment cid tid
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgEnrollment
        idForm <- newIdent
        $(widgetFile "exams/enrollment")


formEnrollment :: CandidateId -> TestId -> Form (CandidateId, TestId, Int)
formEnrollment cid tid extra = do
    attempt <- liftHandler $ runDB $ maybe 1 (+1) . (unValue =<<) <$> selectOne ( do
          x <- from $ table @Exam
          where_ $ x ^. ExamTest ==. val tid
          where_ $ x ^. ExamCandidate ==. val cid
          return $ max_ $ x ^. ExamAttempt )
      
    return (pure (cid,tid,attempt), [whamlet|^{extra}|])


getSearchExamR :: UserId -> ExamId -> Handler Html
getSearchExamR uid eid = do
    stati <- reqGetParams <$> getRequest
    
    exam <- runDB $ selectOne $ do
        (x :& c :& t) <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Test `on` (\(x :& _ :& e) -> x ^. ExamTest ==. e ^. TestId)
        where_ $ x ^. ExamId ==. val eid
        return (x,c,t)

    total <- fromMaybe 0 . (unValue =<<) <$> case exam of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    score <- fromMaybe 0 . (unValue =<<) <$> case exam of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          where_ $ x ^. OptionId `in_` subSelectList
             ( from $ selectQuery $ do
                   y <- from $ table @Answer
                   where_ $ y ^. AnswerExam ==. val eid
                   return $ y ^. AnswerOption
             )
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/search/exam")


getExamsAfterLoginR :: Handler Html
getExamsAfterLoginR = do
    uid <- maybeAuthId
    case uid of
      Just uid' -> redirect $ ExamsR uid'
      Nothing -> do
          addMessageI msgError MsgLoginRequired
          redirect ExamsLoginR


getExamsLoginR :: Handler Html
getExamsLoginR = do
    setUltDest ExamsAfterLoginR
    msgs <- getMessages 
    defaultLayout $ do
        setTitleI MsgAuthenticate
        $(widgetFile "exams/login")


getSearchExamsR :: UserId -> HandlerFor App Html
getSearchExamsR uid = do
    query <- runInputGet $ iopt (searchField True) "q"

    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x

    exams <- case candidate of
      Just (Entity cid _) -> runDB $ select $ queryScores cid query
      Nothing -> return []
          
    setUltDestCurrent
    (fw,et) <- generateFormPost formTests
    defaultLayout $ do
        setTitleI MsgSearch
        idFormQuery <- newIdent
        idButtonBack <- newIdent
        idInputSearch <- newIdent
        idButtonSearch <- newIdent
        idButtonTakeNewExam <- newIdent
        idDialogTests <- newIdent
        $(widgetFile "exams/search/exams")


getExamR :: UserId -> ExamId -> HandlerFor App Html
getExamR uid eid = do 
    exam <- runDB $ selectOne $ do
        (x :& c :& t) <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Test `on` (\(x :& _ :& e) -> x ^. ExamTest ==. e ^. TestId)
        where_ $ x ^. ExamId ==. val eid
        return (x,c,t)

    total <- fromMaybe 0 . (unValue =<<) <$> case exam of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    score <- fromMaybe 0 . (unValue =<<) <$> case exam of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          where_ $ x ^. OptionId `in_` subSelectList
             ( from $ selectQuery $ do
                   y <- from $ table @Answer
                   where_ $ y ^. AnswerExam ==. val eid
                   return $ y ^. AnswerOption
             )
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr (ExamsR uid)) <$> runInputGet (iopt urlField "location")
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/exam")


getExamUserEnrollmentR :: UserId -> TestId -> Handler Html
getExamUserEnrollmentR uid tid = do
    (widget,enctype) <- generateFormPost $ formCandidate uid Nothing
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "exams/candidate/candidate")


formCandidate :: UserId -> Maybe (Entity Candidate) -> Form (Candidate,Maybe FileInfo)
formCandidate uid candidate extra = do
    (fnameR,fnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFamilyName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateFamilyName . entityVal <$> candidate)
        
    (gnameR,gnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgGivenName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateGivenName . entityVal <$> candidate)
        
    (anameR,anameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgAdditionalName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateAdditionalName . entityVal <$> candidate)
        
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateBday . entityVal <$> candidate)
        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    let r = (,) <$> (Candidate <$> fnameR <*> gnameR <*> anameR <*> bdayR <*> pure (Just uid))
                <*> photoR
    
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idOverlay <- newIdent

    idDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idButtonCapture <- newIdent
    
    return ( r
           , $(widgetFile "exams/candidate/form")
           )
    

postExamsR :: UserId -> Handler Html
postExamsR uid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x
          
    setUltDestCurrent
    msgs <- getMessages
    ((fr,fw),et) <- runFormPost formTests
    case fr of
      FormSuccess tid -> do
          case candidate of
            Just (Entity cid _) -> redirect $ ExamEnrollmentFormR uid cid tid
            Nothing -> redirect $ ExamUserEnrollmentR uid tid
              
      _otherwise -> do
          exams <- case candidate of
            Just (Entity cid _) -> runDB $ select $ queryScores cid Nothing
            Nothing -> return []
            
          defaultLayout $ do
              setTitleI MsgMyExams
              idOverlay <- newIdent
              idDialogMainMenu <- newIdent
              idButtonTakeNewExam <- newIdent
              idDialogTests <- newIdent
              let list = $(widgetFile "exams/list")
              $(widgetFile "exams/exams")
    

getExamsR :: UserId -> HandlerFor App Html
getExamsR uid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x

    exams <- case candidate of
      Just (Entity cid _) -> runDB $ select $ queryScores cid Nothing
      Nothing -> return []
          
    setUltDestCurrent
    msgs <- getMessages
    (fw,et) <- generateFormPost formTests
    defaultLayout $ do
        setTitleI MsgMyExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idButtonTakeNewExam <- newIdent
        idDialogTests <- newIdent
        let list = $(widgetFile "exams/list")
        $(widgetFile "exams/exams")


formTests :: Form TestId
formTests extra = do

    tests <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Test
        orderBy [asc (x ^. TestName), asc (x ^. TestId)]
        return (x ^. TestName, x ^. TestId) )
    
    (testR,testV) <- mreq (md3radioField (optionsPairs tests)) FieldSettings
        { fsLabel = SomeMessage MsgTests
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    let w = do
            toWidget [cassius|
                             ##{fvId testV}
                                 display: flex
                                 flex-direction: column
                                 row-gap: 1rem
                             |]
            [whamlet|^{extra} ^{fvInput testV}|]
                
    return (testR,w)


queryScores :: CandidateId -> Maybe Text
            -> SqlQuery ( SqlExpr (Entity Exam)
                        , SqlExpr (Entity Test)
                        , SqlExpr (Value Double)
                        )
queryScores cid query = do
    r :& e :& (_,s) <- from $ table @Exam
       `innerJoin` table @Test `on` (\(r :& e) -> r ^. ExamTest ==. e ^. TestId)
       `innerJoin` ( do
          a :& o <- from $ table @Answer
              `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
          groupBy (a ^. AnswerExam)
          return (a ^. AnswerExam, coalesceDefault [sum_ (o ^. OptionPoints)] (val 0))
        ) `on` (\(r :& _ :& (a, _)) -> a ==. r ^. ExamId)
    where_ $ r ^. ExamCandidate ==. val cid
    case query of
      Just q -> where_ $ upper_ (e ^. TestName) `like` (%) ++. upper_ (val q) ++. (%)
      Nothing -> return ()
    orderBy [desc (r ^. ExamStart), desc (r ^. ExamAttempt)]
    return (r,e,s)


printf :: String -> Double -> String
printf = Printf.printf
