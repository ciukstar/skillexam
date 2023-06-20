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

import qualified Text.Printf as Printf (printf)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (isJust, fromMaybe)
import Text.Hamlet (Html)
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, redirect, lookupSession
    , SomeMessage (SomeMessage), MonadHandler (liftHandler), getUrlRender
    )

import Settings (widgetFile)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvErrors)
    )

import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Fields (hiddenField, intField)
import Yesod.Form.Input (runInputGet, iopt)
import Database.Persist (Entity (Entity, entityKey), PersistStoreWrite (insert))
import Data.Bifunctor (Bifunctor(first, second))
import Data.Text (unpack, pack)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Yesod.Persist.Core (YesodPersist(runDB))

import Foundation
    ( Handler, Widget
    , Route
      ( AdminR, HomeR, ExamR, ExamFormR, ExamTestR
      , StepR, PhotoPlaceholderR
      )
    , AdminR (CandidatePhotoR)
    , AppMessage
      ( MsgExam, MsgCandidate, MsgExam, MsgStartExam
      , MsgCancel, MsgAttempt, MsgPhoto, MsgPoints, MsgName
      , MsgNumberOfQuestions, MsgCode, MsgDuration, MsgMinutes
      , MsgDescr, MsgPassScore, MsgMaxScore, MsgExamInfo
      )
    )

import Database.Esqueleto.Experimental
    ( SqlExpr, select, selectOne, from, table, orderBy, asc, val
    , (^.), (==.), (:&) ((:&))
    , where_, subSelectMaybe, selectQuery, min_, just
    , Value (Value, unValue), max_, on, groupBy, countRows, leftJoin
    , sum_, coalesceDefault
    )

import Model
    ( Exam (Exam)
    , Candidate (Candidate), CandidateId
    , EntityField
      ( CandidateFamilyName, CandidateGivenName, CandidateAdditionalName
      , TestName, StemTest, StemOrdinal, StemId, ExamTest
      , ExamCandidate, ExamAttempt, CandidateId
      , TestId, OptionStem, OptionPoints
      )
    , Test (Test), TestId
    , Stem, userSessKey, ultDestKey, Option
    )


printf :: String -> Double -> String
printf = Printf.printf


getExamTestR :: CandidateId -> TestId -> Handler Html
getExamTestR cid tid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
    test <-(second (((+1) <$>) <$>) <$>) <$> runDB ( selectOne $ do
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


postExamR :: Handler Html
postExamR = do
    ((fr,widget),enctype) <- runFormPost $ formExam Nothing Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
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
      _ -> defaultLayout $ do
          let info = Nothing
          setTitleI MsgExam
          $(widgetFile "exams/enrollment")


postExamFormR :: Handler Html
postExamFormR = do
    ((fr,widget),enctype) <- runFormPost $ formExam Nothing Nothing
    let info = case fr of
          FormSuccess (ExamData tid cid _) -> Just (tid,cid)
          _ -> Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/enrollment")


getExamFormR :: Handler Html
getExamFormR = do
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
      _ -> return Nothing
    let info = meid >>= \eid -> mcid >>= \cid -> return (eid,cid)
    (widget,enctype) <- generateFormPost $ formExam candidate meid
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "exams/enrollment")


formExam :: Maybe (Entity Candidate) -> Maybe TestId
         -> Html -> MForm Handler (FormResult ExamData, Widget)
formExam candidate tid extra = do

    (tR,tV) <- first ((toSqlKey . read . unpack) <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgExam
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (pack . show . fromSqlKey <$> tid)

    (cR,cV) <- first ((toSqlKey . read . unpack) <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgCandidate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (pack . show . fromSqlKey . entityKey <$> candidate)

    tests <- liftHandler $ runDB $ select $ do
        x <- from $ table @Test
        orderBy [asc (x ^. TestName)]
        return x

    candidates <- liftHandler $ runDB $ select $ do
        x <- from $ table @Candidate
        orderBy [ asc (x ^. CandidateFamilyName)
                , asc (x ^. CandidateGivenName)
                , asc (x ^. CandidateAdditionalName)
                ]
        return x
            
    attempt <- maybe 1 (+1) . (unValue =<<) <$> case (tR,cR) of
      (FormSuccess eid', FormSuccess cid) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @Exam
          where_ $ x ^. ExamTest ==. val eid'
          where_ $ x ^. ExamCandidate ==. val cid
          return $ max_ $ x ^. ExamAttempt
      _ -> return $ pure $ Value $ pure 0

    let r = ExamData <$> tR <*> cR <*> pure attempt
    let w = $(widgetFile "exams/form")
            
    return (r,w)


data ExamData = ExamData TestId CandidateId Int
