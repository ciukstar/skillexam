{-# LANGUAGE TemplateHaskell #-}

module Handler.RemoteExams
  ( getRemoteExamR
  ) where

import Data.UUID (UUID)

import Foundation (Handler, AppMessage (MsgRemoteExam))

import Text.Hamlet (Html)

import Settings (widgetFile)

import Yesod.Core (Yesod(defaultLayout), setTitleI)


getRemoteExamR :: UUID -> Handler Html
getRemoteExamR token = do
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "remote/exam")
