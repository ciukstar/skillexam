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

import Control.Monad (unless)

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Text.Printf as Printf (printf)
import Data.Text (pack, unpack, intercalate)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value, unValue), select, from, table, orderBy
    , (^.), (==.), (:&) ((:&)), (>=.)
    , asc, where_, in_, valList, innerJoin, on, groupBy, sum_
    , coalesceDefault, val, desc, selectOne, countRows, having, selectQuery
    , countDistinct
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( widgetMainMenu, widgetAccount, Handler
    , Route (StatsR, DataR, PhotoPlaceholderR, StaticR)
    , StatsR
      ( TopSkilledR, SkilledR, TopExamR, TopExamsR, TestSuccessRateR, ExamSuccessRatesR
      )
    , DataR (CandidatePhotoR)
    , AppMessage
      ( MsgTopSkilled, MsgSkills, MsgCancel, MsgSelect
      , MsgPhoto, MsgRating, MsgCandidate, MsgTopExams
      , MsgNumberOfExaminees, MsgExam, MsgPopularity, MsgDescr
      , MsgSuccessRate, MsgPassRate
      , MsgExamSuccessRate, MsgPass, MsgFail, MsgPassedFailedExamNumber
      , MsgNoExamsYet, MsgTotalCandidates
      )
    )

import Model
    ( Skill(Skill, skillName)
    , Candidate (Candidate), CandidateId
    , Answer
    , Exam
    , Option
    , Stem, Skills (Skills)
    , Test (Test, testName), TestId
    , EntityField
      ( SkillName, SkillId, AnswerExam, ExamId, ExamCandidate
      , CandidateId, AnswerOption, OptionId, OptionStem, StemId, StemSkill
      , CandidateFamilyName, CandidateGivenName, CandidateAdditionalName
      , OptionPoints, OptionKey, ExamTest, TestId, TestName, TestPass
      )
    )
    
import Settings (widgetFile)
import Settings.StaticFiles (echarts_pie_bars_5_4_2_min_js, echarts_gauge_5_4_2_min_js)
    
import Text.Julius (rawJS)
import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), lookupGetParams, setUltDestCurrent
    , RenderMessage (renderMessage), addScript, getYesod, languages, newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (intField, textField, unTextarea)
import Yesod.Persist (Entity (Entity, entityVal))
import Yesod.Persist.Core (YesodPersist(runDB))


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
        idPieChart <- newIdent
        idBarChart <- newIdent
        addScript $ StaticR echarts_pie_bars_5_4_2_min_js
        $(widgetFile "stats/rate")


getExamSuccessRatesR :: Handler Html
getExamSuccessRatesR = do
    mtid <- (toSqlKey <$>) <$> runInputGet ( iopt intField "tid" )

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
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "stats/success-rates")


getTopExamR :: TestId -> Handler Html
getTopExamR tid = do
    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x

    examinees <- maybe 0 unValue <$> runDB ( selectOne $ do
        e <- from $ table @Exam
        where_ $ e ^. ExamTest ==. val tid
        return (countDistinct (e ^. ExamCandidate) :: SqlExpr (Value Double)) )

    candidates <- maybe 1 unValue <$> runDB ( selectOne $ do
        from (table @Candidate) >> return (countRows :: SqlExpr (Value Double)) )

    defaultLayout $ do
        setTitleI MsgExam
        idGauge <- newIdent
        addScript $ StaticR echarts_gauge_5_4_2_min_js
        $(widgetFile "stats/exam")


getTopExamsR :: Handler Html
getTopExamsR = do          
    tests <- zip [1::Int .. ] <$> runDB ( select $ do
        e :& t <- from $ table @Exam
            `innerJoin` table @Test
            `on` (\(r :& t) -> r ^. ExamTest ==. t ^. TestId)
        groupBy (t ^. TestId, t ^. TestName)
        let cr = countDistinct (e ^. ExamCandidate) :: SqlExpr (Value Double)
        orderBy [desc cr]
        return ((t ^. TestId, t ^. TestName), cr) )

    total <- maybe 1 unValue <$> runDB ( selectOne $ do
        from (table @Candidate) >> return (countRows :: SqlExpr (Value Double)) )
        
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgTopExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
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
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "stats/skilled")


getTopSkilledR :: Handler Html
getTopSkilledR = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    sids <- (toSqlKey . read . unpack <$>) <$> lookupGetParams "sid"
    mcid <- (toSqlKey <$>) <$> runInputGet (iopt intField "cid")
    
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
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idDialogSkills <- newIdent
        idFormSkills <- newIdent
        $(widgetFile "stats/top-skilled")


printf :: String -> Double -> String
printf = Printf.printf
