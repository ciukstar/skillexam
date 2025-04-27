{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.RemoteExams
  ( getRemoteExamR
  , getRemoteExamRegisterR
  ) where

import Data.UUID (UUID)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), selectOne, from, table, where_, val, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    , coalesceDefault, sum_, subSelectUnsafe
    )
import Database.Persist (Entity (Entity), entityVal)

import Model
    ( Candidate (Candidate)
    , Remote(Remote, remoteCandidate)
    , Test (Test), TimeUnit (TimeUnitMinute, TimeUnitHour)
    , Option, Stem
    , EntityField
      ( RemoteToken, RemoteTest, TestId, CandidateId, OptionStem, StemId
      , StemTest, OptionPoints, OptionKey
      )
    )

import Foundation
    ( Handler
    , Route (HomeR, PhotoPlaceholderR, DataR, RemoteExamRegisterR)
    , DataR (CandidatePhotoR)
    , AppMessage
      ( MsgRemoteExam, MsgAppName, MsgNoExamFoundAtThisLink, MsgWelcome
      , MsgThisIsTheStartPageOfTheExam, MsgExamName, MsgExamDuration
      , MsgExamCode, MsgMinutes, MsgHours, MsgPassMark, MsgDescr, MsgPoints
      , MsgTotalPoints, MsgPoints, MsgCandidate, MsgRegisterForExam
      , MsgPhoto, MsgPleaseRegisterAsCandidateForExam
      ), Form
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, FileInfo)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Functions (generateFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Text (Text)


getRemoteExamRegisterR :: UUID -> Handler Html
getRemoteExamRegisterR token = do

    (fw,et) <- generateFormPost formRegister
    
    defaultLayout $ do
        setTitleI MsgRemoteExam
        $(widgetFile "remote/candidate/new")


formRegister :: Form ((Candidate,Maybe FileInfo),[Text])
formRegister medias candidate extra = do
    (fnameR,fnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFamilyName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (gnameR,gnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgGivenName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (anameR,anameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgAdditionalName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
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
      
      fieldListOptions = (bimap ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) unValue <$>)

      nameSocialMediaLink :: Text
      nameSocialMediaLink = "social-media-link"

      md3widgetDeleteButton :: FieldView App -> WidgetFor App ()
      md3widgetDeleteButton v = [whamlet|
        <div.field.label.border.round.small.suffix :isJust (fvErrors v):.invalid #idField#{fvId v}>

          ^{fvInput v}
          
          <label for=#{fvId v}>
            #{fvLabel v}
            $if fvRequired v
              <sup>*

          <button.small.circle.transparent type=button style="position: absolute; inset: 10% 0px auto auto;"
              onclick="document.getElementById('idField#{fvId v}').remove()">
            <i.error-text>delete

          $maybe err <- fvErrors v
            <span.error>#{err}
      |]


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
