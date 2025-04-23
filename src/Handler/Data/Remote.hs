{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Data.Remote
  ( getRemotesR
  , getRemoteR
  , getRemoteNewR
  , getRemoteEditR
  , postRemoteDeleR 
  ) where

import Control.Monad.IO.Class (liftIO)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    , where_, val
    )

import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, Form, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR)
    , DataR
      ( RemotesR, RemoteNewR, RemoteR, RemoteEditR, RemoteDeleR
      )
    , AppMessage
      ( MsgRemoteExams, MsgNoTestsYet, MsgAdd, MsgRemoteExam, MsgSave
      , MsgCancel, MsgExamLink, MsgEdit, MsgName, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgCode, MsgDelete, MsgBack, MsgRemoteTests
      , MsgRemoteTest
      )
    )
    
import Model
    ( RemoteId, Remote(Remote)
    , Test (Test)
    , EntityField
      ( RemoteTimeCreated, RemoteTest, TestId, RemoteId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, newIdent, getMessages)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Functions (generateFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Time.Clock (getCurrentTime)


postRemoteDeleR :: RemoteId -> Handler Html
postRemoteDeleR rid = undefined


getRemoteEditR :: RemoteId -> Handler Html
getRemoteEditR rid = undefined


getRemoteR :: RemoteId -> Handler Html
getRemoteR rid = do

    test <- runDB $ selectOne $ do
        x :& t <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
        where_ $ x ^. RemoteId ==. val rid
        return (x,t)

    (fw0,et0) <- generateFormPost formRemoteTestDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExam
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/remote/test")


formRemoteTestDelete :: Form ()
formRemoteTestDelete extra = return (pure (), [whamlet|^{extra}|])


getRemoteNewR :: Handler Html
getRemoteNewR = do

    (fw,et) <- generateFormPost $ formRemoteTest Nothing
    
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "data/remote/new")


formRemoteTest :: Maybe (Entity Remote) -> Form Remote
formRemoteTest remote extra = do

    
    
    now <- liftIO getCurrentTime
    let r = Remote <$> testR <*> candidateR <*> tokenR <*> pure now <*> validR
    let w = undefined
    return (r,w)


getRemotesR :: Handler Html
getRemotesR = do

    tests <- runDB $ select $ do
        x :& t <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
        orderBy [desc (x ^. RemoteTimeCreated)]
        return (x,t)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExams
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/remote/tests") 
