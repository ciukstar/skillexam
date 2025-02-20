{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.MyExams
  ( getMyExamsR
  , getMyExamR
  , getMyExamsSearchR
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (unpack, Text, pack)
import Data.Time.Format.ISO8601 (iso8601Show)

import Foundation
  ( App
  , Route
    ( DataR, MyExamsR, MyExamR, PhotoPlaceholderR, SignInR
    , SignOutR, ExamFormR, MyExamsSearchR
    )
  , DataR (CandidatePhotoR)
  , AppMessage
    ( MsgTakeNewExam, MsgMyExams, MsgNoExams, MsgLoginPrompt
    , MsgLogin, MsgPhoto, MsgLogout, MsgAttempt, MsgExam, MsgBack
    , MsgExamResults, MsgStatus, MsgPass, MsgFail, MsgScore, MsgPassScore
    , MsgMaxScore, MsgCandidate, MsgCompleted, MsgSearch, MsgNoExamsFoundFor
    ), widgetAccount, widgetMainMenu
  )

import Database.Esqueleto.Experimental
    ( SqlQuery, SqlExpr, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&)), (%), (++.)
    , select, orderBy, desc, on, innerJoin, sum_
    , Value (Value, unValue), in_, like
    , subSelectList, selectQuery, upper_
    , groupBy, coalesceDefault, just
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Model
    ( userSessKey
    , Candidate (candidateAdditionalName, candidateGivenName, candidateFamilyName)
    , CandidateId
    , EntityField
      ( CandidateId, ExamCandidate, ExamStart
      , ExamAttempt, ExamTest, TestId, ExamId
      , StemId, StemTest, OptionStem, OptionPoints, OptionId, AnswerOption
      , AnswerExam, TestName, CandidateUser
      )
    , Exam (Exam, examEnd)
    , Test (Test, testPass, testName)
    , ExamId, Option, Stem, Answer, UserId
    )

import Settings ( widgetFile )

import qualified Text.Printf as Printf (printf)

import Yesod.Core (Html, Yesod (defaultLayout), setTitleI)
import Yesod.Core.Handler
    ( HandlerFor, lookupSession, setUltDestCurrent, getUrlRender, newIdent
    , getCurrentRoute
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (searchField, intField, textField, urlField)
import Yesod.Persist.Core (YesodPersist(runDB))


getMyExamsSearchR :: UserId -> HandlerFor App Html
getMyExamsSearchR uid = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "id")
    mrid <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    mq <- runInputGet $ iopt (searchField True) "q"
    mcid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey

    tests <- case mcid of
      Just cid -> runDB $ select $ queryScores cid mq
      Nothing -> return []
          
    setUltDestCurrent
    curr <- fromMaybe (MyExamsSearchR uid) <$> getCurrentRoute
    let list = $(widgetFile "my-exams/list")
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "my-exams/search")


printf :: String -> Double -> String
printf = Printf.printf


getMyExamR :: UserId -> ExamId -> HandlerFor App Html
getMyExamR uid rid = do
    test <- runDB $ selectOne $ do
        (x :& c :& e) <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Test `on` (\(x :& _ :& e) -> x ^. ExamTest ==. e ^. TestId)
        where_ $ x ^. ExamId ==. val rid
        return (x,c,e)

    total <- fromMaybe 0 . (unValue =<<) <$> case test of
      Just (_,_,Entity eid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val eid
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    score <- fromMaybe 0 . (unValue =<<) <$> case test of
      Just (_,_,Entity eid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val eid
          where_ $ x ^. OptionId `in_` subSelectList
             ( from $ selectQuery $ do
                   y <- from $ table @Answer
                   where_ $ y ^. AnswerExam ==. val rid
                   return $ y ^. AnswerOption
             )
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr (MyExamsR uid)) <$> runInputGet (iopt urlField "location")
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "my-exams/my-exam")
    

getMyExamsR :: UserId -> HandlerFor App Html
getMyExamsR uid = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "id")
    mrid <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateUser ==. just (val uid)
        return x

    tests <- case candidate of
      Just (Entity cid _) -> runDB $ select $ queryScores cid Nothing
      Nothing -> return []
          
    setUltDestCurrent
    let list = $(widgetFile "my-exams/list")
    defaultLayout $ do
        setTitleI MsgMyExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "my-exams/my-exams") 


queryScores :: CandidateId -> Maybe Text
            -> SqlQuery ( SqlExpr (Entity Exam)
                        , SqlExpr (Entity Test)
                        , SqlExpr (Value Double)
                        )
queryScores cid mq = do
    r :& e :& (_,s) <- from $ table @Exam
       `innerJoin` table @Test `on` (\(r :& e) -> r ^. ExamTest ==. e ^. TestId)
       `innerJoin` ( do
          a :& o <- from $ table @Answer
              `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
          groupBy (a ^. AnswerExam)
          return (a ^. AnswerExam, coalesceDefault [sum_ (o ^. OptionPoints)] (val 0))
        ) `on` (\(r :& _ :& (a, _)) -> a ==. r ^. ExamId)
    where_ $ r ^. ExamCandidate ==. val cid
    case mq of
      Just q -> where_ $ upper_ (e ^. TestName) `like` (%) ++. upper_ (val q) ++. (%)
      Nothing -> return ()
    orderBy [desc (r ^. ExamStart), desc (r ^. ExamAttempt)]
    return (r,e,s)
