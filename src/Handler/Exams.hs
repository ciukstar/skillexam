{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Exams
  ( getExamFormR
  , postExamFormR
  , postExamR
  , getExamTestR
  ) where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack, Text)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( SqlExpr, select, selectOne, from, table, orderBy, asc, val
    , (^.), (==.), (:&) ((:&))
    , where_, subSelectMaybe, selectQuery, min_, just
    , Value (Value, unValue), max_, on, groupBy, countRows, leftJoin
    , sum_, coalesceDefault
    )
import Database.Persist (Entity (Entity), entityKey, insert)
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler, Form
    , Route
      ( DataR, HomeR, ExamR, ExamFormR, ExamTestR
      , StepR, PhotoPlaceholderR
      )
    , DataR (CandidatePhotoR)
    , AppMessage
      ( MsgExam, MsgCandidate, MsgExam, MsgStartExam
      , MsgCancel, MsgAttempt, MsgPhoto, MsgPoints, MsgName
      , MsgNumberOfQuestions, MsgCode, MsgDuration, MsgMinutes
      , MsgDescr, MsgPassScore, MsgMaxScore, MsgExamInfo
      )
    )

import Material3 (md3selectWidget)

import Model
    ( userSessKey, keyUtlDest, Exam (Exam)
    , CandidateId
    , Candidate
      ( Candidate, candidateGivenName, candidateFamilyName
      , candidateAdditionalName
      )
    , Test (Test, testName), TestId
    , Stem, Option
    , EntityField
      ( CandidateFamilyName, CandidateGivenName, CandidateAdditionalName
      , TestName, StemTest, StemOrdinal, StemId, ExamTest
      , ExamCandidate, ExamAttempt, CandidateId
      , TestId, OptionStem, OptionPoints
      ), UserId
    )
    
import Settings (widgetFile)
    
import Text.Hamlet (Html)
import qualified Text.Printf as Printf (printf)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, redirect, lookupSession
    , SomeMessage (SomeMessage), MonadHandler (liftHandler), getUrlRender, newIdent
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId)
    )
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Fields (intField, selectFieldList)
import Yesod.Persist.Core (YesodPersist(runDB))


getExamTestR :: UserId -> CandidateId -> TestId -> Handler Html
getExamTestR uid cid tid = do
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


printf :: String -> Double -> String
printf = Printf.printf
