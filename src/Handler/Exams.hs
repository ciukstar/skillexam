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

import ClassyPrelude.Yesod (readMay, emailField)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
    ( atomically, readTVar, newBroadcastTChan, writeTVar, writeTChan
    )

import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.List.Safe as LS (head)
import qualified Data.Map as M (insert, delete)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( SqlQuery, SqlExpr, Value (Value, unValue), selectOne, from, table
    , (^.), (==.), (:&) ((:&)), (%), (++.), (>=.), (<.), (/.)
    , where_, val, select, orderBy, innerJoin, sum_, desc, on, in_, like
    , subSelectList, selectQuery, upper_, groupBy, coalesceDefault, just
    , asc, subSelectMaybe, min_, max_, leftJoin, countRows
    )
import Database.Persist (Entity (Entity), entityVal, insert)

import Foundation
  ( App, Handler, Form
  , widgetAccount, widgetMainMenu, widgetSnackbar
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
    , MsgAuthenticate, MsgLoginToSeeYourExamsPlease, MsgEnrollment, MsgTimeout
    , MsgStartExam, MsgSelectATestForTheExam, MsgInvalidFormData, MsgBirthday
    , MsgNoQuestionsForTheTest, MsgPoints, MsgNumberOfQuestions, MsgName
    , MsgMinutes, MsgDuration, MsgDescr, MsgCode, MsgFamilyName, MsgSave
    , MsgGivenName, MsgAdditionalName, MsgUploadPhoto, MsgRetakeThisExam
    , MsgLoginRequired, MsgNoExamsWereFoundForSearchTerms, MsgOngoing
    , MsgTimeCompleted, MsgHours, MsgCancelled, MsgExamsPassed, MsgExamsFailed
    , MsgSortBy, MsgDate, MsgResult, MsgFilter, MsgEmail, MsgPhone
    )
  )
import qualified Foundation as F (App (exams))

import Material3 (md3radioField, md3widget)

import Model
    ( msgError, paramAsc, paramDesc
    , CandidateId
    , Candidate
      ( Candidate, candidateAdditionalName, candidateGivenName, candidateBday
      , candidateFamilyName, candidateEmail, candidatePhone
      )
    , ExamId, Exam (Exam, examEnd, examStatus)
    , ExamStatus
      ( ExamStatusOngoing, ExamStatusCompleted, ExamStatusTimeout, ExamStatusCanceled
      )
    , TestId, Test (Test, testPass, testName)
    , Option, Stem, Answer, UserId
    , TimeUnit (TimeUnitMinute, TimeUnitHour)
    , EntityField
      ( CandidateId, ExamCandidate, ExamStart, ExamAttempt, ExamTest, TestId
      , ExamId, StemId, StemTest, OptionStem, OptionPoints, AnswerOption
      , AnswerExam, TestName, CandidateUser, OptionId, StemOrdinal
      , TestDuration, TestDurationUnit, TestPass, ExamEnd, TestCode
      ), Tokens (Tokens)
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
    , getYesod, lookupGetParam
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

          (duration, unit) <- (maybe (0,TimeUnitMinute) (bimap unValue unValue) <$>) $ runDB $ selectOne $ do
              x <- from $ table @Test
              where_ $ x ^. TestId ==. val tid
              return (x ^. TestDuration, x ^. TestDurationUnit)
              
          case stem of
            Just (Entity qid _) -> do
                ongoing <- F.exams <$> getYesod
                chan <- liftIO $ atomically $ do
                    m <- readTVar ongoing
                    chan <- newBroadcastTChan
                    writeTVar ongoing (M.insert eid chan m)
                    return chan

                _ <- liftIO $ forkIO $ do
                    threadDelay (round (duration * toSeconds unit * 1000000))                        
                    atomically $ writeTChan chan ExamStatusTimeout
                    atomically $ do
                        m <- readTVar ongoing
                        writeTVar ongoing $ M.delete eid m
                        
                setUltDest $ ExamsR uid
                redirect $ StepR cid tid eid qid (Tokens [])
            
            Nothing -> do
                addMessageI msgError MsgNoQuestionsForTheTest
                redirect $ ExamEnrollmentFormR uid cid tid
      
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ ExamEnrollmentFormR uid cid tid

  where
      toSeconds unit = case unit of
        TimeUnitMinute -> 60
        TimeUnitHour -> 3600


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
      Just (Entity cid _) -> do
          result <- (readMay =<<) <$> lookupGetParam paramResult
          sort <- LS.head . filter ((`elem` [paramAsc,paramDesc]) . fst) . reqGetParams <$> getRequest
          runDB $ select $ queryScores cid result sort query
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
        
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateEmail . entityVal <$> candidate)
        
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidatePhone . entityVal <$> candidate)
        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    let r = (,) <$> (Candidate <$> fnameR <*> gnameR <*> anameR <*> bdayR <*> emailR <*> phoneR <*> pure (Just uid))
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
          result <- (readMay =<<) <$> lookupGetParam paramResult
          
          sort <- LS.head . filter ((`elem` [paramAsc,paramDesc]) . fst) . reqGetParams <$> getRequest
          
          exams <- case candidate of
            Just (Entity cid _) -> do
                runDB $ select $ queryScores cid result sort Nothing
            Nothing -> return []
            
          defaultLayout $ do
              setTitleI MsgMyExams 
              idOverlay <- newIdent
              idDialogMainMenu <- newIdent
              idButtonTakeNewExam <- newIdent
              idFormQuery <- newIdent
              idInputResultFilter <- newIdent
              idInputSortBy <- newIdent
              idDialogTests <- newIdent
              let list = $(widgetFile "exams/list")
              $(widgetFile "exams/exams")
    

data ExamResult = ExamResultPassed | ExamResultFailed
    deriving (Eq, Show, Read)


paramResult :: Text
paramResult = "result"

valResult :: Text
valResult = "result"

valExamCode :: Text
valExamCode = "code"

valExamName :: Text
valExamName = "name"

valTimeEnd :: Text
valTimeEnd = "end" 


getExamsR :: UserId -> HandlerFor App Html
getExamsR uid = do

    result <- (readMay =<<) <$> lookupGetParam paramResult

    sort <- LS.head . filter ((`elem` [paramAsc,paramDesc]) . fst) . reqGetParams <$> getRequest
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x

    exams <- case candidate of
      Just (Entity cid _) -> runDB $ select $ queryScores cid result sort Nothing
      Nothing -> return []
          
    setUltDestCurrent
    msgs <- getMessages
    (fw,et) <- generateFormPost formTests
    defaultLayout $ do
        setTitleI MsgMyExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idButtonTakeNewExam <- newIdent
        idInputResultFilter <- newIdent
        idInputSortBy <- newIdent
        idFormQuery <- newIdent
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


queryScores :: CandidateId -> Maybe ExamResult -> Maybe (Text, Text) -> Maybe Text
            -> SqlQuery ( SqlExpr (Entity Exam)
                        , SqlExpr (Entity Test)
                        , (SqlExpr (Value Double), SqlExpr (Value Double))
                        )
queryScores cid result sort query = do
    e :& t :& (_,s) :& (_,n) <- from $ table @Exam
       `innerJoin` table @Test `on` (\(e :& t) -> e ^. ExamTest ==. t ^. TestId)
       `innerJoin` ( do
          a :& o <- from $ table @Answer
              `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
          groupBy (a ^. AnswerExam)
          return (a ^. AnswerExam, coalesceDefault [sum_ (o ^. OptionPoints)] (val 0))
        ) `on` (\(e :& _ :& (a, _)) -> a ==. e ^. ExamId)
       `innerJoin` ( do
          m :& o <- from $ table @Stem
              `innerJoin` table @Option `on` (\(m :& o) -> m ^. StemId ==. o ^. OptionStem)
          groupBy (m ^. StemTest)
          return (m ^. StemTest, coalesceDefault [sum_ (o ^. OptionPoints)] (val 0))
        ) `on` (\(_ :& t :& _ :& (m,_)) -> m ==. t ^. TestId)
        
    where_ $ e ^. ExamCandidate ==. val cid
    
    case query of
      Just q -> where_ $ upper_ (t ^. TestName) `like` (%) ++. upper_ (val q) ++. (%)
      Nothing -> return ()

    case result of
      Just ExamResultPassed -> where_ $ s >=. t ^. TestPass
      Just ExamResultFailed -> where_ $ s <. t ^. TestPass
      Nothing -> return ()

    case sort of
      Just (p,v) | (p,v) == (paramAsc,valTimeEnd) -> orderBy [asc (e ^. ExamEnd)]
                 | (p,v) == (paramDesc,valTimeEnd) -> orderBy [desc (e ^. ExamEnd)]
                 | (p,v) == (paramAsc,valExamName) -> orderBy [asc (t ^. TestName)]
                 | (p,v) == (paramDesc,valExamName) -> orderBy [desc (t ^. TestName)]
                 | (p,v) == (paramAsc,valExamCode) -> orderBy [asc (t ^. TestCode)]
                 | (p,v) == (paramDesc,valExamCode) -> orderBy [desc (t ^. TestCode)]
                 | (p,v) == (paramAsc,valResult) -> orderBy [asc (s /. n)]
                 | (p,v) == (paramDesc,valResult) -> orderBy [desc (s /. n)]
                 | otherwise -> orderBy [desc (e ^. ExamStart), desc (e ^. ExamAttempt)]

      Nothing -> orderBy [desc (e ^. ExamStart), desc (e ^. ExamAttempt)]      
    
    return (e,t,(s,n))


printf :: String -> Double -> String
printf = Printf.printf
