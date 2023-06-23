{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home
  ( getHomeR
  , getSearchExamR
  , getExamInfoR
  , getExamSkillsR
  ) where

import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import qualified Text.Printf as Printf (printf)
import Settings ( widgetFile )
import Yesod.Core (Html, Yesod (defaultLayout), setTitleI)
import Yesod.Form.Fields (Textarea (Textarea), textField)
import Yesod.Core.Handler
    ( HandlerFor, lookupSession, setUltDestCurrent, lookupGetParam
    , getCurrentRoute, getUrlRender
    )

import Foundation
  ( App
  , Route
    ( AdminR, HomeR, SearchExamR, PhotoPlaceholderR, SignInR
    , SignOutR, ExamInfoR, ExamSkillsR, ExamFormR
    )
  , AdminR (CandidatePhotoR)
  , AppMessage
    ( MsgExams, MsgMyExams, MsgDescr, MsgPassMark
    , MsgLogin, MsgPhoto, MsgLogout, MsgSearch, MsgExam, MsgDuration
    , MsgCode, MsgName, MsgTakeThisExam, MsgPopularity, MsgPoints
    , MsgMinutes, MsgDifficulty, MsgDifficultyLow, MsgDetails
    , MsgSkills, MsgNoPublishedExamsYet, MsgDifficultyHigh
    , MsgDifficultyMedium
    )
  )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Yesod.Persist.Core (YesodPersist(runDB))

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, like
    , (^.), (==.), (:&) ((:&)), (%), (++.), (||.)
    , select, orderBy, desc, on, innerJoin, distinct
    , upper_, countRows, Value (Value), selectQuery, crossJoin, countDistinct
    )

import Model
    ( Candidate(Candidate)
    , Test (Test), TestId
    , EntityField
      ( CandidateId
      , TestId, StemTest, StemSkill, SkillId, TestName, TestCode, TestState
      , ExamTest, AnswerOption, OptionId, AnswerExam, ExamId, OptionKey, ExamCandidate
      )
    , userSessKey
    , Stem, Skill (Skill), TestState (TestStatePublished), Exam
    , Answer, Option
    )
import Yesod.Form.Input (runInputGet, iopt)


printf :: String -> Double -> String
printf = Printf.printf


getExamSkillsR :: TestId -> HandlerFor App Html
getExamSkillsR eid = do
    curr <- getCurrentRoute
    location <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupGetParam "location"
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
        let tab = $(widgetFile "home/skills")
        $(widgetFile "home/exam")


getExamInfoR :: TestId -> HandlerFor App Html
getExamInfoR tid = do
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

    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "home/details")
        $(widgetFile "home/exam")


getSearchExamR :: HandlerFor App Html
getSearchExamR = do
    curr <- getCurrentRoute
    mq <- runInputGet $ iopt textField "q"
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    meid <- (toSqlKey . read . unpack <$>) <$> lookupGetParam "eid"
    tests <- runDB $ select $ do
        x <- from $ table @Test
        case mq of
          Just q -> where_ $ (upper_ (x ^. TestName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. TestCode) `like` (%) ++. upper_ (val q) ++. (%))
          _ -> return ()
        orderBy [desc (x ^. TestId)]
        return x
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "home/search")
        $(widgetFile "home/exams")


getHomeR :: HandlerFor App Html
getHomeR = do
    curr <- getCurrentRoute
    mq <- runInputGet $ iopt textField "q"
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    meid <- (toSqlKey . read . unpack <$>) <$> lookupGetParam "eid"
    mcid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
    candidate <- case mcid of
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Candidate
          where_ $ x ^. CandidateId ==. val cid
          return x
      _ -> return Nothing

    tests <- runDB $ select $ do
        x <- from $ table @Test
        where_ $ x ^. TestState ==. val TestStatePublished
        orderBy [desc (x ^. TestId)]
        return x

    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgMyExams
        $(widgetFile "home/home")
        $(widgetFile "home/exams")
