{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Data.Remote
  ( getRemotesR, postRemoteNewExamR
  , getRemoteR
  , getRemoteNewTestR, postRemoteNewTestR
  , getRemoteNewCandidatesR, postRemoteNewCandidatesR
  , getRemoteNewExamR
  , getRemoteEditR
  , postRemoteDeleR 
  ) where

import Control.Monad (unless, forM)
import Control.Monad.IO.Class (liftIO)

import qualified Data.List as L (find)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    , where_, val, asc, in_, valList
    )

import Database.Persist (Entity (Entity), entityVal, insert)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR, RemoteExamR)
    , DataR
      ( RemotesR, RemoteNewTestR, RemoteR, RemoteEditR, CandidatePhotoR
      , RemoteNewCandidatesR, RemoteNewExamR, UserPhotoR, RemoteDeleR
      )
    , AppMessage
      ( MsgRemoteExams, MsgNoTestsYet, MsgAdd, MsgRemoteExam
      , MsgCancel, MsgExamLink, MsgEdit, MsgName, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgCode, MsgDelete, MsgBack, MsgRemoteTests
      , MsgRemoteTest, MsgCandidates, MsgTest, MsgExam, MsgUnpublished
      , MsgNext, MsgPhoto, MsgPublished, MsgPoints, MsgNoCandidatesYet
      , MsgPassMark, MsgMinutes, MsgHours, MsgExamState, MsgDuration
      , MsgDescr, MsgDateCreated, MsgAuthor, MsgYes, MsgNo, MsgValid
      , MsgYouAreAboutToCreateExamForUnregisteredCandidates, MsgCreate
      , MsgYouAreAboutToCreateExamForCandidate, MsgDetails, MsgClose
      , MsgYouAreAboutToCreateExamForCandidates, MsgCandidate
      , MsgNoRegisteredCandidatesHaveBeenSelected, MsgCopyLink
      , MsgLinksForExamsGenerated
      )
    )
    
import Model
    ( msgSuccess
    , RemoteId
    , Remote
      ( Remote, remoteTest, remoteToken, remoteTimeCreated, remoteOwner
      , remoteCandidate, remoteValid
      )
    , CandidateId, Candidate (Candidate), Candidates (Candidates)
    , TimeUnit (TimeUnitMinute, TimeUnitHour)
    , UserId, User (User)
    , TestState (TestStatePublished, TestStateUnpublished)
    , TestId, Test (Test)
    , EntityField
      ( RemoteTimeCreated, RemoteTest, TestId, RemoteId, CandidateFamilyName
      , TestName, CandidateId, CandidateGivenName, CandidateAdditionalName
      , RemoteOwner, UserId
      )
    )

import Settings (widgetFile)

import Text.Cassius (cassius)
import Text.Hamlet (Html)

import Yesod.Auth (YesodAuth(maybeAuthId))
import Yesod.Core
    ( MonadHandler (liftHandler), Yesod(defaultLayout), setTitleI, newIdent
    , getMessages, handlerToWidget, redirect, notAuthenticated, getUrlRender
    , addMessageI
    )
import Yesod.Core.Widget (whamlet, toWidget)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Form.Fields
    ( OptionList (olOptions), Option (optionInternalValue, optionExternalValue)
    , optionsPairs, radioField', multiSelectField
    )
import Yesod.Form.Types
    ( Field(fieldView), FieldView (fvInput)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postRemoteDeleR :: RemoteId -> Handler Html
postRemoteDeleR rid = undefined


getRemoteEditR :: RemoteId -> Handler Html
getRemoteEditR rid = undefined


getRemoteR :: RemoteId -> Handler Html
getRemoteR rid = do

    test <- runDB $ selectOne $ do
        x :& t :& o <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
            `innerJoin` table @User `on` (\(x :& _ :& o) -> x ^. RemoteOwner ==. o ^. UserId)
        where_ $ x ^. RemoteId ==. val rid
        return (x,(t,o))

    (fw0,et0) <- generateFormPost formRemoteTestDelete

    rndr <- getUrlRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExam
        idAnchorExamLink <- newIdent
        idButtonExamLink <- newIdent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/remote/test") 


formRemoteTestDelete :: Form ()
formRemoteTestDelete extra = return (pure (), [whamlet|^{extra}|])


postRemoteNewExamR :: TestId -> Candidates -> Handler Html
postRemoteNewExamR tid candis@(Candidates cids) = do

    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x

    candidates <- runDB $ select $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId `in_` valList cids
        return x
    
    ((fr,fw),et) <- runFormPost formRemoteExam
    case fr of
      FormSuccess uid | null candidates -> do
                        token <- liftIO nextRandom
                        now <- liftIO getCurrentTime 
                        eid <- runDB $ insert Remote { remoteOwner = uid
                                                     , remoteTest = tid
                                                     , remoteCandidate = Nothing
                                                     , remoteToken = token
                                                     , remoteTimeCreated = now
                                                     , remoteValid = True
                                                     }

                        addMessageI msgSuccess MsgLinksForExamsGenerated

                        tests <- runDB $ select $ do
                            x :& t <- from $ table @Remote
                                `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
                            where_ $ x ^. RemoteId ==. val eid
                            return (x,t)

                        rndr <- getUrlRender
                        msgs <- getMessages
                        defaultLayout $ do
                            setTitleI MsgRemoteExam
                            idAnchorExamLink <- newIdent
                            idButtonExamLink <- newIdent
                            $(widgetFile "data/remote/new/links")

                      | otherwise -> do
                            eids <- forM cids $ \cid -> do
                                token <- liftIO nextRandom
                                now <- liftIO getCurrentTime 
                                runDB $ insert Remote { remoteOwner = uid
                                                      , remoteTest = tid
                                                      , remoteCandidate = Just cid
                                                      , remoteToken = token
                                                      , remoteTimeCreated = now
                                                      , remoteValid = True
                                                      }

                            addMessageI msgSuccess MsgLinksForExamsGenerated

                            tests <- runDB $ select $ do
                                x :& t <- from $ table @Remote
                                    `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
                                where_ $ x ^. RemoteId `in_` valList eids
                                return (x,t)

                            rndr <- getUrlRender
                            msgs <- getMessages
                            defaultLayout $ do
                                setTitleI MsgRemoteExam
                                idAnchorExamLink <- newIdent
                                idButtonExamLink <- newIdent
                                $(widgetFile "data/remote/new/links")
              
      _otherwise -> do
          msgs <- getMessages 
          defaultLayout $ do
              setTitleI MsgRemoteExam
              idFormRemoteNewExam <- newIdent
              $(widgetFile "data/remote/new/exam")


getRemoteNewExamR :: TestId -> Candidates -> Handler Html
getRemoteNewExamR tid candis@(Candidates cids) = do

    test <- runDB $ selectOne $ do
        x <- from $ table @Test
        where_ $ x ^. TestId ==. val tid
        return x

    candidates <- runDB $ select $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId `in_` valList cids
        return x
    
    (fw,et) <- generateFormPost formRemoteExam
    
    msgs <- getMessages 
    defaultLayout $ do
        setTitleI MsgRemoteExam
        idFormRemoteNewExam <- newIdent
        $(widgetFile "data/remote/new/exam")


formRemoteExam :: Form UserId
formRemoteExam extra = do
    uid <- maybeAuthId
    case uid of
      Nothing -> notAuthenticated
      Just uid' -> return (pure uid', [whamlet|^{extra}|])


postRemoteNewCandidatesR :: TestId -> Handler Html
postRemoteNewCandidatesR tid = do

    ((fr,fw),et) <- runFormPost $ formRemoteCandidates tid
    case fr of
      FormSuccess (tid',cids') -> redirect $ DataR $ RemoteNewExamR tid' (Candidates cids')
      _otherwise -> do
          msgs <- getMessages 
          defaultLayout $ do
              setTitleI MsgRemoteExam
              idFormRemoteNewTest <- newIdent
              $(widgetFile "data/remote/new/candidates")


getRemoteNewCandidatesR :: TestId -> Handler Html
getRemoteNewCandidatesR tid = do

    (fw,et) <- generateFormPost $ formRemoteCandidates tid

    msgs <- getMessages 
    defaultLayout $ do
        setTitleI MsgRemoteExam
        idFormRemoteNewTest <- newIdent
        $(widgetFile "data/remote/new/candidates")


formRemoteCandidates :: TestId -> Form (TestId,[CandidateId])
formRemoteCandidates tid extra = do
    
    options <- liftHandler $ runDB $ select $ do
        x <- from $ table @Candidate
        orderBy [ asc (x ^. CandidateFamilyName)
                , asc (x ^. CandidateGivenName)
                , asc (x ^. CandidateAdditionalName)
                , asc (x ^. CandidateId)
                ]
        return x

    (candidatesR,candidatesV) <- mopt (md3checkFieldList options) "" Nothing
    
    return ( (,) tid . fromMaybe [] <$> candidatesR
           , [whamlet|^{extra} ^{fvInput candidatesV}|]
           )

  where

      pairs (Entity cid' (Candidate fname gname aname _ _ _ _)) = ( fname <> " " <> gname <> case aname of
                                                                      Just x -> " " <> x
                                                                      Nothing -> ""
                                                                  , cid'
                                                                  )

      md3checkFieldList :: [Entity Candidate] -> Field Handler [CandidateId]
      md3checkFieldList options = (multiSelectField (optionsPairs (pairs <$> options)))
          { fieldView = \theId name attrs x isReq -> do
                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> options))

                let sel (Left _) _ = False
                    sel (Right ys) opt = optionInternalValue opt `elem` ys

                let findOpt :: Option CandidateId -> [Entity Candidate] -> Maybe (Entity Candidate)
                    findOpt opt = L.find (\(Entity cid' _) -> cid' == optionInternalValue opt)
                    
                unless (null opts) $ toWidget [cassius|
                    div.row
                        .content
                            display: inline-grid
                        .headline
                            display: inline-block
                            white-space: nowrap
                            overflow: hidden
                            text-overflow: ellipsis
                    |]
                [whamlet|
                    $if null opts
                        <figure style="text-align:center">
                          <span style="font-size:4rem">&varnothing;
                          <figcaption>
                            _{MsgNoCandidatesYet}.
                    $else
                      <div *{attrs}>
                        $forall (i,opt) <- opts
                          $maybe (Entity cid (Candidate fname gname aname _ _ _ _)) <- findOpt opt options
                            <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">

                              <img.circle src=@{DataR $ CandidatePhotoR cid} alt=_{MsgPhoto} loading=lazy>

                              <div.content.max>
                                <h6.headline.large-text>
                                  #{fname} #{gname}
                                  $maybe aname <- aname
                                    #{aname}

                              <label.checkbox>
                                <input type=checkbox ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                                  :sel x opt:checked :isReq:required>
                                <span>

                |]
                    }


postRemoteNewTestR :: Handler Html
postRemoteNewTestR = do

    ((fr,fw),et) <- runFormPost $ formRemoteTest Nothing
    case fr of
      FormSuccess r -> redirect $ DataR $ RemoteNewCandidatesR r
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRemoteExam
              idFormRemoteNewTest <- newIdent
              $(widgetFile "data/remote/new/test")


getRemoteNewTestR :: Handler Html
getRemoteNewTestR = do

    (fw,et) <- generateFormPost $ formRemoteTest Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExam
        idFormRemoteNewTest <- newIdent
        $(widgetFile "data/remote/new/test")


formRemoteTest :: Maybe (Entity Remote) -> Form TestId
formRemoteTest remote extra = do
    
    options <- liftHandler $ runDB $ select $ do
        x <- from $ table @Test
        orderBy [asc (x ^. TestName), asc (x ^. TestId)]
        return x

    (testR,testV) <- mreq (md3radioFieldList options) "" (remoteTest . entityVal <$> remote)
    
    return (testR, [whamlet|^{extra} ^{fvInput testV}|])

  where

      pairs (Entity tid' (Test _ name _ _ _ _ _)) = (name, tid')

      md3radioFieldList :: [Entity Test] -> Field Handler TestId
      md3radioFieldList options = (radioField' (optionsPairs (pairs <$> options)))
          { fieldView = \theId name attrs x isReq -> do
                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> options))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findOpt :: Option TestId -> [Entity Test] -> Maybe (Entity Test)
                    findOpt opt = L.find (\(Entity tid' _) -> tid' == optionInternalValue opt)
                    
                unless (null opts) $ toWidget [cassius|
                    div.row
                        .content
                            display: inline-grid
                        .headline
                            display: inline-block
                            white-space: nowrap
                            overflow: hidden
                            text-overflow: ellipsis
                    |]
                [whamlet|
                    $if null opts
                        <figure style="text-align:center">
                          <span style="font-size:4rem">&varnothing;
                          <figcaption>
                            _{MsgNoTestsYet}.
                    $else
                      <div *{attrs}>
                        $forall (i,opt) <- opts
                          $maybe (Entity _ (Test tcode tname _ _ _ _ _)) <- findOpt opt options
                            <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">

                              <div.content.max>
                                <h6.headline.large-text>
                                  #{tname}
                                <div.supporting-text.secondary-text>
                                  #{tcode}

                              <label.radio>
                                <input type=radio ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                                  :sel x opt:checked :isReq:required=true>
                                <span>

                |]
                    }


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
