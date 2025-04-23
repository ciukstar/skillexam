{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Data.Remote
  ( getRemotesR
  , getRemoteR
  , getRemoteNewR
  ) where

import Database.Esqueleto.Experimental
    ( select, from, table, where_, orderBy, desc, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    )

import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR)
    , DataR (RemoteNewR, RemoteR)
    , AppMessage
      ( MsgRemoteExams, MsgNoExamsYet, MsgAdd
      )
    )
    
import Model
    ( RemoteId, Remote(Remote)
    , Test (Test)
    , EntityField
      ( RemoteTimeCreated, RemoteTest, TestId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, newIdent, getMessages)
import Yesod.Persist.Core (YesodPersist(runDB))


getRemoteNewR :: Handler Html
getRemoteNewR = undefined


getRemoteR :: RemoteId -> Handler Html
getRemoteR rid = undefined 


getRemotesR :: Handler Html
getRemotesR = do

    exams <- runDB $ select $ do
        x :& t <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
        orderBy [desc (x ^. RemoteTimeCreated)]
        return (x,t)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/exams/exams") 
