{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home
  ( getHomeR
  ) where


import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&))
    , Value (unValue), countRows, innerJoin, on, just
    )
import Database.Persist (Entity (Entity))

import Foundation
  ( Handler, widgetMainMenu, widgetSnackbar, widgetAccount
  , Route (TestExamsR, ExamsR, ExamsLoginR)
  , AppMessage
    ( MsgAvailableTests, MsgTestYourSkillsWith, MsgAppName, MsgMyExams
    , MsgWelcome
    )
  )

import Model
    ( Exam, Candidate
    , Test, TestState (TestStatePublished)
    , EntityField
      (TestState, ExamCandidate, CandidateId, CandidateUser
      )
    )

import Settings ( widgetFile )

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Html, Yesod (defaultLayout), setTitleI, newIdent, getMessages
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do

    user <- maybeAuth

    tests <- runDB $ maybe 0 unValue <$> selectOne ( do
        x <- from $ table @Test
        where_ $ x ^. TestState ==. val TestStatePublished
        return (countRows :: SqlExpr (Value Int)) )

    exams <- case user of
      Just (Entity uid _) -> runDB $ maybe 0 unValue <$> selectOne ( do
        _ :& c <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
        where_ $ c ^. CandidateUser ==. just (val uid)
        return (countRows :: SqlExpr (Value Int)) )
        
      Nothing -> return 0

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "homepage")
