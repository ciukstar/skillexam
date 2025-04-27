{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.RemoteExams
  ( getRemoteExamR
  ) where

import Data.UUID (UUID)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), selectOne, from, table, where_, val, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    , coalesceDefault, subSelectMaybe, sum_, subSelect, subSelectUnsafe
    )
import Database.Persist (Entity (Entity), entityVal)

import Model
    ( Candidate (Candidate)
    , Remote(Remote, remoteCandidate)
    , Test (Test), TimeUnit (TimeUnitMinute, TimeUnitHour)
    , Option (Option), Stem (Stem), Key (StemKey)
    , EntityField
      ( RemoteToken, RemoteTest, TestId, CandidateId, OptionStem, StemId
      , StemTest, OptionPoints, OptionKey
      )
    )

import Foundation
    ( Handler
    , Route (HomeR)
    , AppMessage
      ( MsgRemoteExam, MsgAppName, MsgNoExamFoundAtThisLink, MsgWelcome
      , MsgThisIsTheStartPageOfTheExam
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getRemoteExamR :: UUID -> Handler Html
getRemoteExamR token = do

    remote <- runDB $ selectOne $ do
        x :& t <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)

        let total :: SqlExpr (Value Int)
            total = subSelectUnsafe $ do
                o :& s <- from $ table @Option
                    `innerJoin` table @Stem `on` (\(o :& s) -> o ^. OptionStem ==. s ^. StemId)
                where_ $ s ^. StemTest ==. t ^. TestId
                where_ $ o ^. OptionKey
                return $ coalesceDefault [sum_ (o ^. OptionPoints)] (val 0)
            
        where_ $ x ^. RemoteToken ==. val token
        return ((x,t),total)

    candidate <- case remoteCandidate . entityVal . fst . fst =<< remote of
      Nothing -> return Nothing
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Candidate
          where_ $ x ^. CandidateId ==. val cid
          return x
    
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "remote/exam")
