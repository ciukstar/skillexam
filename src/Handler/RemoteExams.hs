{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.RemoteExams
  ( getRemoteExamR
  , getRemoteExamRegisterR, postRemoteExamRegisterR
  , getRemoteExamStartR
  , postRemoteExamEnrollR
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
    ( atomically, readTVar, newBroadcastTChan, writeTVar, writeTChan
    )
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Map as M (insert, delete)
import qualified Data.Set as S (fromList, toList)
import Data.Text (Text)
import qualified Data.Text as T (null, strip)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value, unValue), selectOne, from, table, where_, val
    , (^.), (?.), (==.), (:&) ((:&)), (=.)
    , coalesceDefault, sum_, subSelectUnsafe, innerJoin, on, set
    , update, just, leftJoin, subSelectMaybe, selectQuery, min_, max_
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert, upsert, insertMany_
    )
import qualified Database.Persist as P ((=.))

import Foundation
    ( Form, Handler, widgetSnackbar
    , Route
      ( HomeR, PhotoPlaceholderR, DataR, RemoteExamRegisterR, StaticR
      , RemoteExamR, RemoteExamStartR, RemoteExamEnrollR
      )
    , DataR (CandidatePhotoR)
    , AppMessage
      ( MsgRemoteExam, MsgAppName, MsgNoExamFoundAtThisLink, MsgWelcome
      , MsgThisIsTheStartPageOfTheExam, MsgExamName, MsgExamDuration
      , MsgExamCode, MsgMinutes, MsgHours, MsgPassMark, MsgDescr, MsgPoints
      , MsgTotalPoints, MsgPoints, MsgCandidate, MsgRegisterForExam
      , MsgPhoto, MsgPleaseRegisterAsCandidateForExam, MsgFamilyName
      , MsgGivenName, MsgAdditionalName, MsgBirthday, MsgEmail, MsgPhone
      , MsgUploadPhoto, MsgTakePhoto, MsgSocialMedia, MsgPersonalData
      , MsgNoSocialMediaLinks, MsgContacts, MsgClose, MsgAdd, MsgSave
      , MsgCancel, MsgRegistrationSuccessful, MsgPleaseClickStartExamButton
      , MsgAreYouReadyToStartTheExam, MsgName, MsgExam, MsgDuration
      , MsgStartExam, MsgInvalidFormData, MsgNoQuestionsForTheTest
      )
    )
import qualified Foundation as F (App (exams))
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , CandidateId, Candidate (Candidate)
    , Exam (Exam), ExamStatus (ExamStatusOngoing, ExamStatusTimeout)
    , RemoteId, Remote(remoteCandidate)
    , TestId, Test (Test), TimeUnit (TimeUnitMinute, TimeUnitHour)
    , Option, Stem
    , Social (Social), Photo (Photo)
    , EntityField
      ( RemoteToken, RemoteTest, TestId, CandidateId, OptionStem, StemId
      , StemTest, OptionPoints, OptionKey, PhotoPhoto, PhotoMime, RemoteId
      , RemoteCandidate, StemOrdinal, TestDuration, TestDurationUnit, ExamTest
      , ExamCandidate, ExamAttempt
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, FileInfo, SomeMessage (SomeMessage)
    , newIdent, getMessageRender, lookupPostParams, getMessages, redirect
    , addMessageI, getYesod, setUltDest, MonadHandler (liftHandler)
    )
import Yesod.Core.Handler (fileSourceByteString, fileContentType)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (textField, dayField, emailField, fileField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( FieldView (fvId, fvErrors, fvInput), FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postRemoteExamEnrollR :: RemoteId -> CandidateId -> TestId -> UUID -> Handler Html
postRemoteExamEnrollR rid cid tid token = do
    
    ((fr,_),_) <- runFormPost $ formEnrollment cid tid
    case fr of
      FormSuccess (cid',tid', attempt) -> do
          now <- liftIO getCurrentTime
          eid <- runDB $ insert (Exam tid' cid' ExamStatusOngoing attempt now Nothing)
          stem <- runDB $ selectOne $ do
              x <- from $ table @Stem
              where_ $ x ^. StemTest ==. val tid
              where_ $ just (x ^. StemOrdinal) ==. subSelectMaybe ( from $ selectQuery $ do
                  y <- from $ table @Stem
                  where_ $ y ^. StemTest ==. x ^. StemTest
                  return $ min_ $ y ^. StemOrdinal )
              return x

          (duration, unit) <- (maybe (0,TimeUnitMinute) (bimap unValue unValue) <$>) $ runDB $ selectOne $ do
              x <- from $ table @Test
              where_ $ x ^. TestId ==. val tid
              return (x ^. TestDuration, x ^. TestDurationUnit)
              
          case stem of
            Just (Entity qid _) -> do
                ongoing <- F.exams <$> getYesod
                chan <- liftIO $ atomically $ do
                    m <- readTVar ongoing
                    chan <- newBroadcastTChan
                    writeTVar ongoing (M.insert eid chan m)
                    return chan

                _ <- liftIO $ forkIO $ do
                    threadDelay (round (duration * toSeconds unit * 1000000))                        
                    atomically $ writeTChan chan ExamStatusTimeout
                    atomically $ do
                        m <- readTVar ongoing
                        writeTVar ongoing $ M.delete eid m
                        
                setUltDest $ RemoteExamR token
                undefined -- redirect $ StepR uid tid eid qid
            
            Nothing -> do
                addMessageI msgError MsgNoQuestionsForTheTest
                redirect $ RemoteExamR token
      
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ RemoteExamR token

  where
      toSeconds unit = case unit of
        TimeUnitMinute -> 60
        TimeUnitHour -> 3600


formEnrollment :: CandidateId -> TestId -> Form (CandidateId, TestId, Int)
formEnrollment cid tid extra = do
    attempt <- liftHandler $ runDB $ maybe 1 (+1) . (unValue =<<) <$> selectOne ( do
          x <- from $ table @Exam
          where_ $ x ^. ExamTest ==. val tid
          where_ $ x ^. ExamCandidate ==. val cid
          return $ max_ $ x ^. ExamAttempt )
      
    return (pure (cid,tid,attempt), [whamlet|^{extra}|])


getRemoteExamStartR :: RemoteId -> CandidateId -> UUID -> Handler Html
getRemoteExamStartR rid cid token = do

    remote <- runDB $ selectOne $ do
        x :& t :& c <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)
            `leftJoin` table @Candidate `on` (\(x :& _ :& c) -> x ^. RemoteCandidate ==. c ?. CandidateId)
        where_ $ x ^. RemoteId ==. val rid
        where_ $ x ^. RemoteToken ==. val token
        where_ $ c ?. CandidateId ==. just (val cid)
        return (x,(t,c))
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "remote/start")


postRemoteExamRegisterR :: RemoteId -> UUID -> Handler Html
postRemoteExamRegisterR rid token = do

    ((fr,fw),et) <- runFormPost formRegister

    case fr of
      FormSuccess ((c,Just fi),links) -> do
          cid <- runDB $ insert c
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (Photo cid bs (fileContentType fi))
              [ PhotoPhoto P.=. bs
              , PhotoMime P.=. fileContentType fi
              ]

          runDB $ insertMany_ (Social cid <$> links)

          runDB $ update $ \x -> do
              set x [RemoteCandidate =. just (val cid)]
              where_ $ x ^. RemoteId ==. val rid
              
          addMessageI msgSuccess MsgRegistrationSuccessful
          redirect $ RemoteExamStartR rid cid token
          
      FormSuccess ((c,Nothing),links) -> do
          cid <- runDB $ insert c
          runDB $ insertMany_ (Social cid <$> links)

          runDB $ update $ \x -> do
              set x [RemoteCandidate =. just (val cid)]
              where_ $ x ^. RemoteId ==. val rid
              
          addMessageI msgSuccess MsgRegistrationSuccessful
          redirect $ RemoteExamStartR rid cid token
          
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRemoteExam 
              $(widgetFile "remote/candidate/new")


getRemoteExamRegisterR :: RemoteId -> UUID -> Handler Html
getRemoteExamRegisterR rid token = do

    (fw,et) <- generateFormPost formRegister
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "remote/candidate/new")


formRegister :: Form ((Candidate,Maybe FileInfo),[Text])
formRegister extra = do
    (fnameR,fnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFamilyName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (gnameR,gnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgGivenName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (anameR,anameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgAdditionalName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    links <- S.toList . S.fromList . filter (not . T.null . T.strip) <$> lookupPostParams nameSocialMediaLink

    let r = (,) <$> ( (,) <$> (Candidate <$> fnameR <*> gnameR <*> anameR <*> bdayR <*> emailR <*> phoneR <*> pure Nothing)
                          <*> photoR
                    )
                <*> pure links
    
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idOverlay <- newIdent

    idDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idButtonCapture <- newIdent

    idPagePersonalData <- newIdent
    idPageContacts <- newIdent
    idPageSocialMedia <- newIdent
    idSocialMediaLinks <- newIdent
    idFigureEmptySocial <- newIdent
    idButtonSocialAdd <- newIdent

    msgr <- getMessageRender
    
    return ( r
           , $(widgetFile "remote/candidate/form") 
           )
        
  where
      nameSocialMediaLink :: Text
      nameSocialMediaLink = "social-media-link"


getRemoteExamR :: UUID -> Handler Html
getRemoteExamR token = do

    remote <- runDB $ selectOne $ do
        x :& t <- from $ table @Remote
            `innerJoin` table @Test `on` (\(x :& t) -> x ^. RemoteTest ==. t ^. TestId)

        let total :: SqlExpr (Value Double)
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
