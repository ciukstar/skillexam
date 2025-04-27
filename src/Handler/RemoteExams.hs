{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.RemoteExams
  ( getRemoteExamR
  , getRemoteExamRegisterR, postRemoteExamRegisterR
  ) where

import qualified Data.Set as S (fromList, toList)
import Data.Text (Text)
import qualified Data.Text as T (null, strip)
import Data.UUID (UUID)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&))
    , coalesceDefault, sum_, subSelectUnsafe, innerJoin, on
    )
import Database.Persist (Entity (Entity), entityVal)

import Foundation
    ( Form, Handler
    , Route
      ( HomeR, PhotoPlaceholderR, DataR, RemoteExamRegisterR, StaticR
      , RemoteExamR
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
      , MsgCancel
      )
    )
    
import Material3 (md3widget)

import Model
    ( Candidate (Candidate)
    , Remote(remoteCandidate)
    , Test (Test), TimeUnit (TimeUnitMinute, TimeUnitHour)
    , Option, Stem
    , EntityField
      ( RemoteToken, RemoteTest, TestId, CandidateId, OptionStem, StemId
      , StemTest, OptionPoints, OptionKey
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
    , newIdent, getMessageRender, lookupPostParams
    )
import Yesod.Form.Fields (textField, dayField, emailField, fileField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt)
import Yesod.Form.Types
    ( FieldView (fvId, fvErrors, fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getRemoteExamRegisterR :: UUID -> Handler Html
getRemoteExamRegisterR token = do

    (fw,et) <- generateFormPost formRegister
    
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
