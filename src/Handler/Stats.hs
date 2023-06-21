{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Stats
  ( getTopSkilledR
  , getSkilledR
  , getTopExamsR
  , getTopExamR
  , getExamSuccessRatesR
  , getTestSuccessRateR
  ) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Text.Printf as Printf (printf)
import Control.Monad (unless)
import Data.Text (pack, unpack, intercalate)
import Text.Julius (rawJS)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), lookupGetParams, getUrlRenderParams
    , lookupSession, setUltDestCurrent, addScript
    , RenderMessage (renderMessage), getYesod, languages
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (intField, textField, unTextarea)
import Settings (widgetFile)
import Settings.StaticFiles (echarts_pie_bars_5_4_2_min_js, echarts_gauge_5_4_2_min_js)

import Foundation
    ( Handler
    , Route (StatsR, AdminR, PhotoPlaceholderR, SignInR, SignOutR, StaticR)
    , StatsR (TopSkilledR, SkilledR, TopExamR, TopExamsR, TestSuccessRateR, ExamSuccessRatesR)
    , AdminR (CandidatePhotoR)
    , AppMessage
      ( MsgTopSkilled, MsgSkills, MsgCancel, MsgSelect
      , MsgPhoto, MsgRating, MsgCandidate, MsgTopExams
      , MsgNumberOfExaminees, MsgExam, MsgPopularity, MsgDescr
      , MsgTotal, MsgSuccessRate, MsgLogin, MsgLogout, MsgPassRate
      , MsgExamSuccessRate, MsgPass, MsgFail, MsgPassedFailedExamNumber
      , MsgNoExamsYet
      )
    )

import Yesod.Persist (Entity (Entity, entityVal))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value, unValue), select, from, table, orderBy
    , (^.), (==.), (:&) ((:&)), (>=.)
    , asc, where_, in_, valList, innerJoin, on, groupBy, sum_
    , coalesceDefault, val, desc, selectOne, countRows, having, selectQuery
    )

import Model
    ( userSessKey
    , Skill(Skill, skillName)
    , EntityField
      ( SkillName, SkillId, AnswerExam, ExamId, ExamCandidate
      , CandidateId, AnswerOption, OptionId, OptionStem, StemId, StemSkill
      , CandidateFamilyName, CandidateGivenName, CandidateAdditionalName
      , OptionPoints, OptionKey, ExamTest, TestId, TestName, TestPass
      )
    , Candidate (Candidate), CandidateId
    , Answer
    , Exam
    , Option
    , Stem, Skills (Skills)
    , Test (Test, testName), TestId
    )


getTestSuccessRateR :: TestId -> Handler Html
getTestSuccessRateR tid = do
    app <- getYesod
    langs <- languages
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x

    let calc = maybe (0,0,0,0) (\(_,_,Value c,Value d) -> (c,d - c,c / d,(d - c) / d))
        
    (npass,nfail,rpass,rfail) <- calc <$> runDB ( selectOne ( do
        (testid,tname,passed) :& (_,total)  <- from ( ( selectQuery ( do
                (_, testid, tname, _, _)  <- from ( selectQuery $ do
                    _ :& o :& e :& t <- from $ table @Answer
                        `innerJoin` table @Option
                        `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                        `innerJoin` table @Exam
                        `on` (\(a :& _ :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        `innerJoin` table @Test
                        `on` (\(_ :& _ :& e :& t) -> e ^. ExamTest ==. t ^. TestId)

                    where_ $ o ^. OptionKey
                    where_ $ t ^. TestId ==. val tid
                    let score = coalesceDefault [sum_ (o ^. OptionPoints)] (val 0) :: SqlExpr (Value Double)
                    groupBy (e ^. ExamId, t ^. TestId, t ^. TestName, t ^. TestPass)
                    having $ score >=. t ^. TestPass
                    return (e ^. ExamId, t ^. TestId, t ^. TestName, t ^. TestPass, score) )
                groupBy (testid,tname)
                return (testid,tname, countRows :: SqlExpr (Value Double)) )
            ) `innerJoin` ( selectQuery ( do
                    e <- from $ table @Exam
                    where_ $ e ^. ExamTest ==. val tid
                    groupBy (e ^. ExamTest)
                    return (e ^. ExamTest, countRows :: SqlExpr (Value Double))
                ) ) `on` (\((testid,_,_) :& (testid',_)) -> testid ==. testid') )

        return (testid,tname,passed,total)

        ) )
    defaultLayout $ do
        setTitleI MsgPassRate
        addScript $ StaticR echarts_pie_bars_5_4_2_min_js
        $(widgetFile "stats/rate")


getExamSuccessRatesR :: Handler Html
getExamSuccessRatesR = do
    mtid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "tid" )
    user <- do
        muid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
        case muid of
          Just uid -> runDB $ selectOne $ do
              x <- from $ table @Candidate
              where_ $ x ^. CandidateId ==. val uid
              return x
          Nothing -> return Nothing

    let trans = zip [1::Int ..]
            . sortBy (\(_,_,_,_,e) (_,_,_,_,e') -> compare e' e)
            . ((\(Value a,Value b,Value c,Value d) -> (a,b,c,d, c / d)) <$>)
    passed <- trans <$> runDB ( select ( do
        (tid,tname,passed) :& (_,total)  <- from ( ( selectQuery ( do
                (_, tid, tname, _, _)  <- from ( selectQuery $ do
                    _ :& o :& e :& t <- from $ table @Answer
                        `innerJoin` table @Option
                        `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                        `innerJoin` table @Exam
                        `on` (\(a :& _ :& e) -> a ^. AnswerExam ==. e ^. ExamId)
                        `innerJoin` table @Test
                        `on` (\(_ :& _ :& e :& t) -> e ^. ExamTest ==. t ^. TestId)

                    where_ $ o ^. OptionKey
                    let score = coalesceDefault [sum_ (o ^. OptionPoints)] (val 0) :: SqlExpr (Value Double)
                    groupBy (e ^. ExamId, t ^. TestId, t ^. TestName, t ^. TestPass)
                    having $ score >=. t ^. TestPass
                    return (e ^. ExamId, t ^. TestId, t ^. TestName, t ^. TestPass, score) )
                groupBy (tid,tname)
                return (tid,tname, countRows :: SqlExpr (Value Double)) )
            ) `innerJoin` ( selectQuery ( do
                    e <- from $ table @Exam
                    groupBy (e ^. ExamTest)
                    return (e ^. ExamTest, countRows :: SqlExpr (Value Double))
                ) ) `on` (\((tid,_,_) :& (tid',_)) -> tid ==. tid') )

        return (tid,tname,passed,total)

        ) )

    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgSuccessRate
        $(widgetFile "stats/success-rates")


getTopExamR :: TestId -> Handler Html
getTopExamR tid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x

    attempts <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ :& e <- from $ table @Exam
            `innerJoin` table @Test
            `on` (\(r :& e) -> r ^. ExamTest ==. e ^. TestId)
        where_ $ e ^. TestId ==. val tid
        return (countRows :: SqlExpr (Value Double)) )

    total <- maybe 1 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Exam
        return (countRows :: SqlExpr (Value Double)) )

    defaultLayout $ do
        setTitleI MsgExam
        addScript $ StaticR echarts_gauge_5_4_2_min_js
        $(widgetFile "stats/exam")


getTopExamsR :: Handler Html
getTopExamsR = do
    user <- do
        muid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
        case muid of
          Just uid -> runDB $ selectOne $ do
              x <- from $ table @Candidate
              where_ $ x ^. CandidateId ==. val uid
              return x
          Nothing -> return Nothing
          
    tests <- zip [1::Int .. ] <$> runDB ( select $ do
        _ :& e <- from $ table @Exam
            `innerJoin` table @Test
            `on` (\(r :& e) -> r ^. ExamTest ==. e ^. TestId)
        groupBy (e ^. TestId, e ^. TestName)
        let cr = countRows :: SqlExpr (Value Double)
        orderBy [desc cr]
        return ((e ^. TestId, e ^. TestName), cr) )

    total <- maybe 1 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Exam
        return (countRows :: SqlExpr (Value Double)) )
        
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgTopExams
        $(widgetFile "stats/top-exams")


getSkilledR :: CandidateId -> Skills -> Handler Html
getSkilledR cid (Skills sids) = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x

    skills <- runDB (select $ do
        _ :& _ :& c :& o :& q :& s <- from $ table @Answer
            `innerJoin` table @Exam
            `on` (\(a :& r) -> a ^. AnswerExam ==. r ^. ExamId)
            `innerJoin` table @Candidate
            `on` (\(_ :& r :& c) -> r ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Option
            `on` (\(a :& _ :& _ :& o) -> a ^. AnswerOption ==. o ^. OptionId)
            `innerJoin` table @Stem
            `on` (\(_ :& _ :& _ :& o :& q) -> o ^. OptionStem ==. q ^. StemId)
            `innerJoin` table @Skill
            `on` (\(_ :& _ :& _ :& _ :& q :& s) -> q ^. StemSkill ==. s ^. SkillId)
        where_ $ c ^. CandidateId ==. val cid
        where_ $ o ^. OptionKey
        unless (null sids) $ where_ (q ^. StemSkill `in_` valList sids) 
        groupBy (s ^. SkillId, s ^. SkillName)
        let score = coalesceDefault [sum_ (o ^. OptionPoints)] (val (0 :: Double))

        orderBy [desc score]
        
        return ((s ^. SkillId, s ^. SkillName), score) )

    let total = sum (unValue . snd <$> skills)
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    let params = ("cid", (pack . show . fromSqlKey) cid)
            : ("scrollY", scrollY)
            : (("sid",) . pack . show . fromSqlKey <$> sids) 
    rndr <- getUrlRenderParams
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "stats/skilled")


getTopSkilledR :: Handler Html
getTopSkilledR = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    sids <- (toSqlKey . read . unpack <$>) <$> lookupGetParams "sid"
    mcid <- (toSqlKey <$>) <$> runInputGet (iopt intField "cid")
    user <- do
        muid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
        case muid of
          Just uid -> runDB $ selectOne $ do
              x <- from $ table @Candidate
              where_ $ x ^. CandidateId ==. val uid
              return x
          Nothing -> return Nothing
    
    skills <- runDB $ select $ do
        s <- from $ table @Skill
        where_ $ s ^. SkillId `in_` valList sids
        orderBy [asc (s ^. SkillName)]
        return s
    allSkills <- runDB $ select $ do
        s <- from $ table @Skill
        orderBy [asc (s ^. SkillName)]
        return s

    rating <- zip [1::Int ..] <$> runDB ( select $ do
        _ :& _ :& c :& o :& q <- from $ table @Answer
            `innerJoin` table @Exam
            `on` (\(a :& r) -> a ^. AnswerExam ==. r ^. ExamId)
            `innerJoin` table @Candidate
            `on` (\(_ :& r :& c) -> r ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Option
            `on` (\(a :& _ :& _ :& o) -> a ^. AnswerOption ==. o ^. OptionId)
            `innerJoin` table @Stem
            `on` (\(_ :& _ :& _ :& o :& q) -> o ^. OptionStem ==. q ^. StemId)
        where_ $ o ^. OptionKey
        unless (null sids) $ where_ (q ^. StemSkill `in_` valList sids) 
        groupBy ( c ^. CandidateId
                , c ^. CandidateFamilyName
                , c ^. CandidateGivenName
                , c ^. CandidateAdditionalName
                )
        let score = coalesceDefault [sum_ (o ^. OptionPoints)] (val (0 :: Double))

        orderBy [desc score]
        
        return ( c ^. CandidateId
               , c ^. CandidateFamilyName
               , c ^. CandidateGivenName
               , c ^. CandidateAdditionalName
               , score
               ) )
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgTopSkilled
        $(widgetFile "stats/top-skilled")


printf :: String -> Double -> String
printf = Printf.printf
