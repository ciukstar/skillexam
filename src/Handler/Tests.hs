{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Tests
  ( getExamFormR, postExamFormR
  , postExamR
  , getExamTestR
  , getCandiateExamTestEnrollFormR
  , getCandiateExamTestEnrollR
  
  , getExamTestsR
  , getSearchExamR
  , getExamInfoR
  , getTestSkillsR
  , getSearchTestInfoR
  , getSearchExamSkillsR
  , getTestExamLoginR
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, like
    , (^.), (?.), (==.), (:&) ((:&)), (%), (++.), (||.)
    , select, orderBy, desc, on, innerJoin, distinct
    , upper_, countRows, Value (Value), selectQuery, crossJoin
    , countDistinct, just, subSelectMaybe, min_, max_, leftJoin
    , groupBy, sum_, coalesceDefault, unValue, asc
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
  ( Handler, Form, widgetMainMenu, widgetSnackbar, widgetAccount
  , Route
    ( HomeR, SearchExamR, ExamInfoR, SearchTestInfoR, ExamTestsR, StepR
    , TestSkillsR, ExamFormR, SearchExamSkillsR, AuthR, TestExamLoginR
    , DataR, PhotoPlaceholderR, ExamTestR, ExamR, CandiateExamTestEnrollFormR
    , CandiateExamTestEnrollFormR, CandiateExamTestEnrollR
    )
  , DataR (CandidatePhotoR)
  , AppMessage
    ( MsgExams, MsgMyExams, MsgDescr, MsgPassMark, MsgNumberOfQuestions
    , MsgSearch, MsgExam, MsgDuration, MsgBack, MsgLogin, MsgCancel
    , MsgCode, MsgName, MsgTakeThisExam, MsgPopularity, MsgPoints
    , MsgMinutes, MsgDifficulty, MsgDifficultyLow, MsgDetails
    , MsgSkills, MsgNoPublishedExamsYet, MsgDifficultyHigh, MsgMaxScore
    , MsgDifficultyMedium, MsgAuthenticate, MsgYouNeedToLoginForExam
    , MsgLoginRequired, MsgStartExam, MsgExamInfo, MsgPassScore, MsgPhoto
    , MsgCandidate, MsgAttempt, MsgInvalidArguments
    )
  )

import Material3 (md3selectWidget)

import Model
    ( keyUtlDest, userSessKey
    , CandidateId, Candidate (Candidate)
    , Stem
    , Skill (Skill)
    , Exam (Exam), Answer, Option
    , TestId, Test (Test, testName), TestState (TestStatePublished)
    , UserId
    , EntityField
      ( TestId, StemTest, StemSkill, SkillId, TestName, TestCode, TestState
      , ExamTest, AnswerOption, OptionId, AnswerExam, OptionKey, ExamCandidate
      , ExamId, CandidateId, StemOrdinal, StemId, OptionStem, OptionPoints
      , ExamAttempt, CandidateFamilyName, CandidateGivenName, CandidateUser
      , CandidateAdditionalName
      )
    )

import Settings ( widgetFile )

import qualified Text.Printf as Printf (printf)

import Yesod.Auth (maybeAuth, Route (LoginR), YesodAuth (maybeAuthId))
import Yesod.Core
    ( Html, Yesod (defaultLayout), setTitleI, newIdent, getMessages
    , YesodRequest (reqGetParams), getRequest, setUltDestCurrent
    , redirect, lookupSession, liftHandler, SomeMessage (SomeMessage), setUltDest, invalidArgsI
    )
import Yesod.Core.Handler
    ( lookupGetParam, getCurrentRoute, getUrlRender
    )
import Yesod.Form.Fields
    ( Textarea (Textarea), textField, selectFieldList, intField )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Functions (runFormPost, mreq, generateFormPost)
import Yesod.Form.Types
    ( FormResult(FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getCandiateExamTestEnrollFormR  :: TestId -> UserId -> Handler Html
getCandiateExamTestEnrollFormR tid uid = undefined


getCandiateExamTestEnrollR :: TestId -> Handler Html
getCandiateExamTestEnrollR tid = do
    uid <- maybeAuthId
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. val uid
        return x

    case candidate of
      Just (Entity cid (Candidate _ _ _ _ (Just uid'))) -> redirect $ ExamTestR tid uid' cid
      
      Just (Entity _ (Candidate _ _ _ _ Nothing)) -> invalidArgsI [MsgInvalidArguments]
      
      Nothing -> redirect $ TestExamLoginR tid
    


getExamTestR :: TestId -> UserId -> CandidateId -> Handler Html
getExamTestR tid uid cid = do
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
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/exam") 


postExamR :: UserId -> Handler Html
postExamR uid = do
    idForm <- newIdent
    ((fr,widget),enctype) <- runFormPost $ formExam idForm Nothing Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
    case fr of
      FormSuccess (ExamData tid cid attempt) -> do
          let info = Just (tid,cid)
          now <- liftIO getCurrentTime
          rid <- runDB $ insert (Exam tid cid attempt now Nothing)
          mqid <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe ( from $ selectQuery $ do
                  y <- from $ table @Stem
                  where_ $ y ^. StemTest ==. x ^. StemTest
                  return $ min_ $ y ^. StemOrdinal )
              return $ x ^. StemId
          case mqid of
            Just (Value qid) -> redirect $ StepR tid rid qid
            
            Nothing -> defaultLayout $ do
                setTitleI MsgExam
                $(widgetFile "exams/enrollment")
                
      _otherwise -> defaultLayout $ do
          let info = Nothing
          setTitleI MsgExam
          $(widgetFile "exams/enrollment")


postExamFormR :: UserId -> Handler Html
postExamFormR uid = do
    idForm <- newIdent
    ((fr,widget),enctype) <- runFormPost $ formExam idForm Nothing Nothing
    let info = case fr of
          FormSuccess (ExamData tid cid _) -> Just (tid,cid)
          
          _otherwise -> Nothing
          
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/enrollment")


getExamFormR :: UserId -> Handler Html
getExamFormR uid = do
    mcid <- do
        x <- (toSqlKey <$>) <$> runInputGet (iopt intField "cid")
        y <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
        return $ x <|> y
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    
    candidate <- case mcid of
      Just cid -> runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
        
      _otherwise -> return Nothing
      
    let info = meid >>= \eid -> mcid >>= \cid -> return (eid,cid)
    idForm <- newIdent
    (widget,enctype) <- generateFormPost $ formExam idForm candidate meid
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/enrollment")


formExam :: Text -> Maybe (Entity Candidate) -> Maybe TestId -> Form ExamData
formExam idForm candidate tid extra = do

    candidates <- liftHandler $ runDB $ select $ do
        x <- from $ table @Candidate
        orderBy [ asc (x ^. CandidateFamilyName)
                , asc (x ^. CandidateGivenName)
                , asc (x ^. CandidateAdditionalName)
                , asc (x ^. CandidateId)
                ]
        return x

    (candidateR,candidateV) <- mreq (selectFieldList ((\(Entity eid e) -> (candidateName e, eid)) <$> candidates)) FieldSettings
        { fsLabel = SomeMessage MsgCandidate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (entityKey <$> candidate)

    tests <- liftHandler $ runDB $ select $ do
        x <- from $ table @Test
        orderBy [asc (x ^. TestName), asc (x ^. TestId)]
        return x

    (testR,testV) <- mreq (selectFieldList ((\(Entity eid e) -> (testName e, eid)) <$> tests)) FieldSettings
        { fsLabel = SomeMessage MsgExam
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } tid
            
    attempt <- maybe 1 (+1) . (unValue =<<) <$> case (testR,candidateR) of
      (FormSuccess eid', FormSuccess cid) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @Exam
          where_ $ x ^. ExamTest ==. val eid'
          where_ $ x ^. ExamCandidate ==. val cid
          return $ max_ $ x ^. ExamAttempt
          
      _otherwise -> return $ pure $ Value $ pure 0

    let r = ExamData <$> testR <*> candidateR <*> pure attempt
    let w = $(widgetFile "exams/form")
            
    return (r,w)

  where
      candidateName (Candidate fn gn (Just an) _ _) = gn <> " " <> fn <> " " <> an
      candidateName (Candidate fn gn Nothing _ _) = gn <> " " <> fn


data ExamData = ExamData !TestId !CandidateId !Int


getTestExamLoginR :: TestId -> Handler Html
getTestExamLoginR tid = do
    setUltDest $ CandiateExamTestEnrollR tid
    defaultLayout $ do
        setTitleI MsgAuthenticate
        $(widgetFile "tests/login")


getSearchExamSkillsR :: TestId -> Handler Html
getSearchExamSkillsR eid = do
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


getSearchTestInfoR :: TestId -> Handler Html
getSearchTestInfoR tid = do
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
    location <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupGetParam "location"
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
    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/skills")
        $(widgetFile "tests/test")


getExamInfoR :: TestId -> Handler Html
getExamInfoR tid = do
    user <- maybeAuth
    curr <- getCurrentRoute
    location <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupGetParam "location"
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
    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "tests/details")
        $(widgetFile "tests/test")


getSearchExamR :: Handler Html
getSearchExamR = do
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
        $(widgetFile "tests/search/tests")


getExamTestsR :: Handler Html
getExamTestsR = do
    mq <- runInputGet $ iopt textField "q"

    tests <- runDB $ select $ do
        x <- from $ table @Test
        where_ $ x ^. TestState ==. val TestStatePublished
        orderBy [desc (x ^. TestId)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMyExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "tests/tests")


printf :: String -> Double -> String
printf = Printf.printf
