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
  , getSearchExamInfoR
  , getSearchExamSkillsR
  ) where

import Data.Text (pack)
import Data.Maybe (fromMaybe)

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, like
    , (^.), (==.), (:&) ((:&)), (%), (++.), (||.)
    , select, orderBy, desc, on, innerJoin, distinct
    , upper_, countRows, Value (Value), selectQuery, crossJoin
    , countDistinct
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
  ( Handler, widgetMainMenu, widgetSnackbar, widgetAccount
  , Route
    ( HomeR, SearchExamR, ExamInfoR, SearchExamInfoR
    , ExamSkillsR, ExamFormR, SearchExamSkillsR
    )
  , AppMessage
    ( MsgExams, MsgMyExams, MsgDescr, MsgPassMark
    , MsgSearch, MsgExam, MsgDuration
    , MsgCode, MsgName, MsgTakeThisExam, MsgPopularity, MsgPoints
    , MsgMinutes, MsgDifficulty, MsgDifficultyLow, MsgDetails
    , MsgSkills, MsgNoPublishedExamsYet, MsgDifficultyHigh
    , MsgDifficultyMedium
    )
  )

import Model
    ( Candidate
    , Stem
    , Skill (Skill)
    , TestState (TestStatePublished)
    , Exam, Answer, Option
    , Test (Test), TestId
    , EntityField
      ( TestId, StemTest, StemSkill, SkillId, TestName, TestCode, TestState
      , ExamTest, AnswerOption, OptionId, AnswerExam, OptionKey, ExamCandidate
      , ExamId
      )
    )

import Settings ( widgetFile )

import qualified Text.Printf as Printf (printf)

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Html, Yesod (defaultLayout), setTitleI, newIdent, getMessages
    , YesodRequest (reqGetParams), getRequest
    )
import Yesod.Core.Handler
    ( lookupGetParam, getCurrentRoute, getUrlRender
    )
import Yesod.Form.Fields (Textarea (Textarea), textField)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Persist.Core (YesodPersist(runDB))


printf :: String -> Double -> String
printf = Printf.printf


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
        let tab = $(widgetFile "home/skills")
        $(widgetFile "home/search/exam/exam")


getSearchExamInfoR :: TestId -> Handler Html
getSearchExamInfoR tid = do
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
        let tab = $(widgetFile "home/details")
        $(widgetFile "home/search/exam/exam")


getExamSkillsR :: TestId -> Handler Html
getExamSkillsR eid = do
    user <- maybeAuth
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

    defaultLayout $ do
        setTitleI MsgExam
        let tab = $(widgetFile "home/details")
        $(widgetFile "home/exam")


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
        $(widgetFile "home/search/search")


getHomeR :: Handler Html
getHomeR = do
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
        $(widgetFile "home/home")
