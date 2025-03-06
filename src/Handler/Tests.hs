{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Tests
  ( getTestExamsR
  , getSearchTestExamsR
  , getTestExamR
  , getTestSkillsR
  , getSearchTestExamR
  , getSearchTestExamSkillsR
  , getTestExamLoginR
  
  , getTestExamEnrollmentR
  , getTestExamUserEnrollmentR
  , getTestExamEnrollmentFormR, postTestExamEnrollmentFormR
  ) where

import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, like
    , (^.), (==.), (:&) ((:&)), (%), (++.), (||.)
    , select, orderBy, desc, on, innerJoin, distinct
    , upper_, countRows, Value (Value), selectQuery, crossJoin
    , countDistinct, just, subSelectMaybe, min_, max_, leftJoin
    , groupBy, sum_, coalesceDefault, unValue
    )
import Database.Persist (Entity (Entity), insert)

import Foundation
  ( Handler, Form, widgetMainMenu, widgetSnackbar, widgetAccount
  , Route
    ( SearchTestExamsR, TestExamR, SearchTestExamR, TestExamsR, StepR
    , TestSkillsR, SearchTestExamSkillsR, AuthR, TestExamLoginR
    , DataR, PhotoPlaceholderR, TestExamEnrollmentFormR
    , TestExamEnrollmentR, TestExamUserEnrollmentR
    )
  , DataR (CandidatePhotoR)
  , AppMessage
    ( MsgExams, MsgDescr, MsgPassMark, MsgNumberOfQuestions
    , MsgSearch, MsgExam, MsgDuration, MsgBack, MsgLogin, MsgCancel
    , MsgCode, MsgName, MsgTakeThisExam, MsgPopularity, MsgPoints
    , MsgMinutes, MsgDifficulty, MsgDifficultyLow, MsgDetails
    , MsgSkills, MsgNoPublishedExamsYet, MsgDifficultyHigh, MsgMaxScore
    , MsgDifficultyMedium, MsgAuthenticate, MsgYouNeedToLoginForExam
    , MsgLoginRequired, MsgStartExam, MsgPassScore, MsgPhoto
    , MsgCandidate, MsgAttempt, MsgInvalidArguments, MsgNoQuestionsForTheTest
    , MsgInvalidFormData, MsgNoExamsWereFoundForSearchTerms
    )
  )

import Model
    ( msgError
    , CandidateId, Candidate (Candidate)
    , Stem
    , Skill (Skill)
    , Exam (Exam), Answer, Option
    , TestId, Test (Test), TestState (TestStatePublished)
    , UserId
    , EntityField
      ( TestId, StemTest, StemSkill, SkillId, TestName, TestCode, TestState
      , ExamTest, AnswerOption, OptionId, AnswerExam, OptionKey, ExamCandidate
      , ExamId, CandidateId, StemOrdinal, StemId, OptionStem, OptionPoints
      , ExamAttempt, CandidateUser
      )
    )

import Settings ( widgetFile )

import qualified Text.Printf as Printf (printf)

import Yesod.Auth (maybeAuth, Route (LoginR), YesodAuth (maybeAuthId))
import Yesod.Core
    ( Html, Yesod (defaultLayout), setTitleI, newIdent, getMessages
    , YesodRequest (reqGetParams), getRequest, setUltDestCurrent
    , redirect, liftHandler
    , setUltDest, invalidArgsI, whamlet, addMessageI
    )
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Form.Fields (Textarea (Textarea), textField)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Functions (runFormPost, generateFormPost)
import Yesod.Form.Types (FormResult(FormSuccess))
import Yesod.Persist.Core (YesodPersist(runDB))


getTestExamUserEnrollmentR  :: TestId -> UserId -> Handler Html
getTestExamUserEnrollmentR tid uid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x

    case candidate of
      Just (Entity cid (Candidate _ _ _ _ (Just uid'))) -> redirect $ TestExamEnrollmentFormR tid uid' cid
      
      Just (Entity _ (Candidate _ _ _ _ Nothing)) -> invalidArgsI [MsgInvalidArguments]
      
      Nothing -> redirect $ TestExamLoginR tid


getTestExamEnrollmentR :: TestId -> Handler Html
getTestExamEnrollmentR tid = do
    uid <- maybeAuthId
    case uid of
      Just uid' -> redirect $ TestExamUserEnrollmentR tid uid'      
      Nothing   -> redirect $ TestExamLoginR tid


postTestExamEnrollmentFormR :: TestId -> UserId -> CandidateId -> Handler Html
postTestExamEnrollmentFormR tid uid cid = do
    
    ((fr,_),_) <- runFormPost $ formEnrollment cid tid
    case fr of
      FormSuccess (cid',tid', attempt) -> do
          now <- liftIO getCurrentTime
          eid <- runDB $ insert (Exam tid' cid' attempt now Nothing)
          
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
                setUltDest TestExamsR
                redirect $ StepR tid eid qid
            
            Nothing -> do
                addMessageI msgError MsgNoQuestionsForTheTest
                redirect $ TestExamEnrollmentFormR tid uid cid
      
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ TestExamEnrollmentFormR tid uid cid


getTestExamEnrollmentFormR :: TestId -> UserId -> CandidateId -> Handler Html
getTestExamEnrollmentFormR tid uid cid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
        
    test <- (second (((+1) <$>) <$>) <$>) <$> runDB ( selectOne $ do
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
        setTitleI MsgExam
        $(widgetFile "tests/exam")


formEnrollment :: CandidateId -> TestId -> Form (CandidateId, TestId, Int)
formEnrollment cid tid extra = do
    attempt <- liftHandler $ runDB $ maybe 1 (+1) . (unValue =<<) <$> selectOne ( do
          x <- from $ table @Exam
          where_ $ x ^. ExamTest ==. val tid
          where_ $ x ^. ExamCandidate ==. val cid
          return $ max_ $ x ^. ExamAttempt )
      
    return (pure (cid,tid,attempt), [whamlet|^{extra}|]) 


getTestExamLoginR :: TestId -> Handler Html
getTestExamLoginR tid = do
    setUltDest $ TestExamEnrollmentR tid
    defaultLayout $ do
        setTitleI MsgAuthenticate
        $(widgetFile "tests/login")


getSearchTestExamSkillsR :: TestId -> Handler Html
getSearchTestExamSkillsR eid = do
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    curr <- getCurrentRoute
    
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val eid
        return x

    skills <- runDB $ select $ distinct $ do
        x :& q <- from $ table @Skill
            `innerJoin` table @Stem `on` (\(x :& q) -> q ^. StemSkill ==. x ^. SkillId)
        where_ $ q ^. StemTest ==. val eid
        return x

    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/skills")
        $(widgetFile "tests/search/test")


getSearchTestExamR :: TestId -> Handler Html
getSearchTestExamR tid = do
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    curr <- getCurrentRoute
    
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x
    
    (nbr,total,ratio) <- maybe (0,1,0) (\(Value n,Value t) -> (n,t,n / t)) <$> runDB ( selectOne $ do
        (n :& t) <- from $ selectQuery ( do
                        x <- from $ table @Exam
                        where_ $ x ^. ExamTest ==. val tid
                        return (countDistinct (x ^. ExamCandidate) :: SqlExpr (Value Double)) )
               `crossJoin` selectQuery
                    (from (table @Candidate) >> return (countRows :: SqlExpr (Value Double)))
        return (n,t) )

    (_,_,dRatio) <- maybe (0,1,0) (\(Value n,Value t) -> (n,t,n / t)) <$> runDB ( selectOne $ do
        (n :& t) <- from $ selectQuery ( do
                        _ :& o :& e <- from $ table @Answer
                            `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                            `innerJoin` table @Exam `on` (\(a :& _ :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        where_ $ e ^. ExamTest ==. val tid
                        where_ $ o ^. OptionKey
                        return (countRows :: SqlExpr (Value Double)) )
               `crossJoin` selectQuery ( do
                        _ :& e <- from $ table @Answer
                            `innerJoin` table @Exam `on` (\(a :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        where_ $ e ^. ExamTest ==. val tid
                        return (countRows :: SqlExpr (Value Double)) )
        return (n,t) )
    
    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/details")
        $(widgetFile "tests/search/test")


getTestSkillsR :: TestId -> Handler Html
getTestSkillsR tid = do
    user <- maybeAuth
    curr <- getCurrentRoute
    
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x
        
    skills <- runDB $ select $ distinct $ do
        x :& q <- from $ table @Skill
            `innerJoin` table @Stem `on` (\(x :& q) -> q ^. StemSkill ==. x ^. SkillId)
        where_ $ q ^. StemTest ==. val tid
        return x

    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/skills")
        $(widgetFile "tests/test")


getTestExamR :: TestId -> Handler Html
getTestExamR tid = do
    user <- maybeAuth
    curr <- getCurrentRoute
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x
    
    (nbr,total,ratio) <- maybe (0,1,0) (\(Value n,Value t) -> (n,t,n / t)) <$> runDB ( selectOne $ do
        (n :& t) <- from $ selectQuery ( do
                        x <- from $ table @Exam
                        where_ $ x ^. ExamTest ==. val tid
                        return (countDistinct (x ^. ExamCandidate) :: SqlExpr (Value Double)) )
               `crossJoin` selectQuery
                    (from (table @Candidate) >> return (countRows :: SqlExpr (Value Double)))
        return (n,t) )

    (_,_,dRatio) <- maybe (0,1,0) (\(Value n,Value t) -> (n,t,n / t)) <$> runDB ( selectOne $ do
        (n :& t) <- from $ selectQuery ( do
                        _ :& o :& e <- from $ table @Answer
                            `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                            `innerJoin` table @Exam `on` (\(a :& _ :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        where_ $ e ^. ExamTest ==. val tid
                        where_ $ o ^. OptionKey
                        return (countRows :: SqlExpr (Value Double)) )
               `crossJoin` selectQuery ( do
                        _ :& e <- from $ table @Answer
                            `innerJoin` table @Exam `on` (\(a :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        where_ $ e ^. ExamTest ==. val tid
                        return (countRows :: SqlExpr (Value Double)) )
        return (n,t) )

    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/details")
        $(widgetFile "tests/test")


getSearchTestExamsR :: Handler Html
getSearchTestExamsR = do
    stati <- reqGetParams <$> getRequest 
    mq <- runInputGet $ iopt textField "q"
    tests <- runDB $ select $ do
        x <- from $ table @Test
        case mq of
          Just q -> where_ $ (upper_ (x ^. TestName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. TestCode) `like` (%) ++. upper_ (val q) ++. (%))
            
          Nothing -> return ()
          
        orderBy [desc (x ^. TestId)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSearch
        idFormQuery <- newIdent
        idButtonBack <- newIdent
        idInputSearch <- newIdent
        idButtonSearch <- newIdent
        $(widgetFile "tests/search/tests")


getTestExamsR :: Handler Html
getTestExamsR = do

    tests <- runDB $ select $ do
        x <- from $ table @Test
        where_ $ x ^. TestState ==. val TestStatePublished
        orderBy [desc (x ^. TestId)]
        return x

    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "tests/tests")


printf :: String -> Double -> String
printf = Printf.printf
