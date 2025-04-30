{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Data.Candidates
  ( getCandidatesR, postCandidatesR
  , getCandidateNewR
  , getCandidatePhotoR
  , getCandidateR, postCandidateR
  , getCandidateEditFormR
  , postCandidateDeleR
  , getCandidatesSearchR
  , getCandidateExamsR
  , getCandidateExamR
  , getCandidateSkillsR
  , getCandidatePhotosR
  ) where

import Control.Monad (void, forM_, forM)

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S (fromList, toList)
import Data.Text (pack, Text)
import qualified Data.Text as T (take, null, strip, isInfixOf, isPrefixOf)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, innerJoin, on
    , (^.), (==.), (%), (++.), (||.), (:&) ((:&)), (<=.), (&&.)
    , val, selectOne, where_, delete, in_
    , upper_, like, just, Value (Value, unValue)
    , distinct, selectQuery, groupBy, countRows, leftJoin, coalesceDefault
    , SqlExpr, sum_, subSelectList, asc
    )
import Database.Persist
    ( Entity (Entity, entityVal), insert, replace, upsert, insertMany_
    , upsertBy
    )
import qualified Database.Persist as P ((=.))
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App, Form, Handler, widgetAccount, widgetMainMenu, widgetSnackbar
    , Route
      ( StaticR, PhotoPlaceholderR
      , DataR
      )
    , DataR
      ( CandidatesR, CandidateR, CandidateSkillsR, CandidatePhotoR
      , CandidateExamsR, CandidateDeleR, CandidatesSearchR, CandidateNewR
      , CandidateExamR, CandidateEditFormR
      )
    , AppMessage
      ( MsgCandidates, MsgSearch, MsgAdd, MsgFamilyName, MsgGivenName
      , MsgAdditionalName, MsgCandidate, MsgCancel, MsgSave
      , MsgBirthday, MsgPhoto, MsgSkills , MsgDelete, MsgDetails
      , MsgBack, MsgAge, MsgExams, MsgInvalidData, MsgClose, MsgSocialMedia
      , MsgNoCandidatesYet, MsgNewRecordAdded, MsgRecordEdited
      , MsgRecordDeleted, MsgPass, MsgFail, MsgNoSkillsYet, MsgCancelled
      , MsgNoExamsYet, MsgPassExamInvite, MsgTakePhoto, MsgUploadPhoto
      , MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease, MsgInvalidFormData
      , MsgExam, MsgCompleted, MsgExamResults, MsgMaxScore, MsgPassScore
      , MsgScore, MsgStatus, MsgUser, MsgTimeCompleted, MsgStatus, MsgOngoing
      , MsgCompleted, MsgTimeout, MsgEmail, MsgPhone, MsgPersonalData
      , MsgContacts, MsgNoSocialMediaLinks
      )
    )

import Material3 (md3widget, md3widgetSelect)

import Model
    ( msgSuccess, msgError
    , Candidate
      ( Candidate, candidateFamilyName, candidateGivenName, candidateBday
      , candidateAdditionalName, candidateUser, candidateEmail, candidatePhone
      )
    , CandidateId, Photo (Photo)
    , ExamId, Exam (Exam, examEnd, examStatus)
    , ExamStatus
      ( ExamStatusOngoing, ExamStatusCompleted, ExamStatusTimeout
      , ExamStatusCanceled
      )
    , Test (Test, testName, testPass)
    , Stem, Answer, Option, Skill (Skill), User
    , Social (Social, socialLink), Unique (UniqueSocial)
    , EntityField
      ( CandidateId, PhotoCandidate, PhotoPhoto, CandidateFamilyName
      , CandidateGivenName, CandidateAdditionalName, UserEmail
      , ExamTest, ExamCandidate, StemId, TestId, ExamId, AnswerExam
      , OptionId, AnswerOption, OptionStem, StemSkill, SkillId, OptionKey
      , SkillName, OptionPoints, TestPass, StemTest, UserName, UserId
      , PhotoMime, SocialLink, SocialCandidate
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    , img_Logo_Whatsapp_Green_svg, img_Logo_Telegram_svg
    )
    
import Text.Hamlet (Html)
import qualified Text.Printf as Printf (printf)

import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), setTitleI
    , TypedContent (TypedContent), ToContent (toContent), emptyContent
    , liftIO, getUrlRenderParams, MonadHandler (liftHandler)
    , FileInfo (fileContentType), languages, lookupPostParams, getMessageRender
    )
import Yesod.Core.Handler
    ( redirect, fileSourceByteString, getMessages, addMessageI
    , setUltDestCurrent, redirectUltDest, newIdent
    )
import Yesod.Core.Types (WidgetFor)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, dayField, fileField, intField, selectFieldList, emailField, urlField
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost)
import Yesod.Form.Types
    (FormResult (FormSuccess)
    , FieldView (fvInput, fvErrors, fvId, fvRequired, fvLabel)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))


getCandidatesSearchR :: Handler Html
getCandidatesSearchR = do
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    mq <- runInputGet $ iopt textField "q"
    candidates <- runDB $ select $ do
        x :& (_,nexams) <- from $ table @Candidate
            `leftJoin` from ( selectQuery $ do
                                  r <- from $ table @Exam
                                  groupBy (r ^. ExamCandidate)
                                  return (r ^. ExamCandidate, countRows :: SqlExpr (Value Int))
                            ) `on` (\(x :& (r,_)) -> just (x ^. CandidateId) ==. r)
        case mq of
          Just q -> where_ $ (upper_ (x ^. CandidateFamilyName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. CandidateGivenName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. CandidateAdditionalName) `like` (%) ++. upper_ (just (val q)) ++. (%))
          Nothing -> return ()
        orderBy [desc (x ^. CandidateId)]
        return (x, coalesceDefault [nexams] (val 0))

    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSearch
        idFormQuery <- newIdent
        idInputSearch <- newIdent
        idButtonSearch <- newIdent
        $(widgetFile "data/candidates/search")
        $(widgetFile "data/candidates/candidates")


postCandidateDeleR :: CandidateId -> Handler Html
postCandidateDeleR cid = do
        
    ((fr,_),_) <- runFormPost formCandidateDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Candidate
              where_ $ x ^. CandidateId ==. val cid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR CandidatesR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ CandidateR cid


postCandidateR :: CandidateId -> Handler Html
postCandidateR cid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x

    links <- runDB $ select $ do
        x <- from $ table @Social
        where_ $ x ^. SocialCandidate ==. val cid
        return x
        
    ((fr,fw),et) <- runFormPost $ formCandidate links candidate
    case fr of
      FormSuccess ((r,Just fi),ls) -> do
          runDB $ replace cid r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (Photo cid bs (fileContentType fi))
              [ PhotoPhoto P.=. bs
              , PhotoMime P.=. fileContentType fi
              ]
              
          runDB $ delete $ do
              x <- from $ table @Social
              where_ $ x ^. SocialCandidate ==. val cid
              
          runDB $ insertMany_ (Social cid <$> ls)
            
          addMessageI msgSuccess MsgRecordEdited
          redirectUltDest $ DataR CandidatesR
          
      FormSuccess ((r,Nothing),ls) -> do
          runDB $ replace cid r
          forM_ (Social cid <$> ls) $ \x@(Social _ link) -> do
              runDB $ upsertBy (UniqueSocial cid link) x [ SocialLink P.=. link ]
              
          addMessageI msgSuccess MsgRecordEdited
          redirectUltDest $ DataR CandidatesR
          
      _otherwise -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgCandidate
          $(widgetFile "data/candidates/edit")


getCandidateEditFormR :: CandidateId -> Handler Html
getCandidateEditFormR cid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x

    links <- runDB $ select $ do
        x <- from $ table @Social
        where_ $ x ^. SocialCandidate ==. val cid
        return x
        
    (fw,et) <- generateFormPost $ formCandidate links candidate
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgCandidate
        $(widgetFile "data/candidates/edit")


getCandidateSkillsR :: CandidateId -> Handler Html
getCandidateSkillsR cid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
        
    skills <- runDB $ select $ distinct $ do
        _ :& r :& o :& _ :& s <- from $ table @Answer
            `innerJoin` table @Exam `on` (\(a :& r) -> a ^. AnswerExam ==. r ^. ExamId)
            `innerJoin` table @Option `on` (\(a :& _ :& o) -> a ^. AnswerOption ==. o ^. OptionId)
            `innerJoin` table @Stem `on` (\(_ :& _ :& o :& q) -> o ^. OptionStem ==. q ^. StemId)
            `innerJoin` table @Skill `on` (\(_ :& _ :& _ :& q :& s) -> q ^. StemSkill ==. s ^. SkillId)
        where_ $ r ^. ExamCandidate ==. val cid
        where_ $ r ^. ExamTest `in_` subSelectList (
            from $ selectQuery $ do
                  e :& _ <- from $ table @Test `innerJoin` from (
                      selectQuery $ do
                            o' :& q :& _ :& r' <- from $ table @Option
                                `innerJoin` table @Stem
                                    `on` (\(o' :& q) -> o' ^. OptionStem ==. q ^. StemId)
                                `innerJoin` table @Answer
                                    `on` (\(o' :& _ :& a) -> o' ^. OptionId ==. a ^. AnswerOption)
                                `innerJoin` table @Exam
                                    `on` (\(_ :& _ :& a :& r') -> a ^. AnswerExam ==. r' ^. ExamId)
                            where_ $ r' ^. ExamCandidate ==. val cid
                            where_ $ o' ^. OptionKey
                            groupBy (q ^. StemTest)
                            return ( q ^. StemTest
                                   , sum_ (o' ^. OptionPoints) :: SqlExpr (Value (Maybe Double))
                                   )
                      ) `on` (\(e :& (eid,score)) -> e ^. TestId ==. eid &&. just (e ^. TestPass) <=. score)

                  return $ e ^. TestId
            )
        where_ $ o ^. OptionKey
        orderBy [desc (s ^. SkillName)]
        return s
    
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "data/candidates/skills")


getCandidateExamR :: CandidateId -> ExamId -> Handler Html
getCandidateExamR cid eid = do
    test <- runDB $ selectOne $ do
        (x :& c :& e) <- from $ table @Exam
            `innerJoin` table @Candidate `on` (\(x :& c) -> x ^. ExamCandidate ==. c ^. CandidateId)
            `innerJoin` table @Test `on` (\(x :& _ :& e) -> x ^. ExamTest ==. e ^. TestId)
        where_ $ x ^. ExamId ==. val eid
        return (x,c,e)

    total <- fromMaybe 0 . (unValue =<<) <$> case test of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    score <- fromMaybe 0 . (unValue =<<) <$> case test of
      Just (_,_,Entity tid _) -> runDB $ selectOne $ do
          x :& q <- from $ table @Option
             `innerJoin` table @Stem `on` (\(x :& q) -> x ^. OptionStem ==. q ^. StemId)
          where_ $ q ^. StemTest ==. val tid
          where_ $ x ^. OptionId `in_` subSelectList
             ( from $ selectQuery $ do
                   y <- from $ table @Answer
                   where_ $ y ^. AnswerExam ==. val eid
                   return $ y ^. AnswerOption
             )
          return $ sum_ $ x ^. OptionPoints
      Nothing -> return $ pure $ Value $ pure (0 :: Double)

    defaultLayout $ do
        setTitleI MsgExam
        $(widgetFile "data/candidates/exam")


getCandidateExamsR :: CandidateId -> Handler Html
getCandidateExamsR cid = do
    
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
        
    tests <- runDB $ select $ do
        x :& e :& (_,score) <- from $ table @Exam
            `innerJoin` table @Test `on` (\(x :& e) -> x ^. ExamTest ==. e ^. TestId)
            `leftJoin` from ( selectQuery ( do

                a :& o <- from $ table @Answer
                    `innerJoin` table @Option `on` (\(a :& o) -> a ^. AnswerOption ==. o ^. OptionId)
                groupBy (a ^. AnswerExam)
                return ( a ^. AnswerExam
                       , coalesceDefault [sum_ (o ^. OptionPoints)] (val 0) :: SqlExpr (Value Double)
                       ) )

                             ) `on` (\(x :& _ :& (rid,_)) -> just (x ^. ExamId) ==. rid)

        where_ $ x ^. ExamCandidate ==. val cid
        orderBy [desc (x ^. ExamId)]
        return (x,e, coalesceDefault [score] (val 0))
        
    mrid <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    rndr <- getUrlRenderParams
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "data/candidates/exams")


getCandidateR :: CandidateId -> Handler Html
getCandidateR cid = do
    
    candidate <- do
        candidate <- runDB $ selectOne $ do
            x <- from $ table @Candidate
            where_ $ x ^. CandidateId ==. val cid
            return x
        y <- liftIO $ (\(y,_,_) -> y) . toGregorian .  utctDay <$> getCurrentTime
        return $ candidate >>= \c -> return (c, (y -) . (\(y',_,_) -> y') . toGregorian <$> (candidateBday . entityVal) c)

    links <- runDB $ select $ do
        x <- from $ table @Social
        where_ $ x ^. SocialCandidate ==. val cid
        return x

    (fw0,et0) <- generateFormPost formCandidateDelete

    langs <- languages
    msgs <- getMessages    
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgCandidate
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/candidates/candidate")


formCandidateDelete :: Form ()
formCandidateDelete extra = return (pure (), [whamlet|^{extra}|]) 


getCandidatePhotosR :: Handler TypedContent
getCandidatePhotosR = do
    mcid <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    photo <- case mcid of
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Photo
          where_ $ x ^. PhotoCandidate ==. val cid
          return x
      Nothing -> return Nothing
    return $ case photo of
      Just (Entity _ (Photo _ bs _)) -> TypedContent "image/avif" $ toContent bs
      Nothing -> TypedContent "image/avif" emptyContent


getCandidatePhotoR :: CandidateId -> Handler TypedContent
getCandidatePhotoR cid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Photo
        where_ $ x ^. PhotoCandidate ==. val cid
        return x
    return $ case photo of
      Just (Entity _ (Photo _ bs _)) -> TypedContent "image/avif" $ toContent bs
      Nothing -> TypedContent "image/avif" emptyContent


postCandidatesR :: Handler Html
postCandidatesR = do
    ((fr,fw),et) <- runFormPost $ formCandidate [] Nothing
    case fr of
      FormSuccess ((r,Just fi),links) -> do
          cid <- runDB $ insert r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (Photo cid bs (fileContentType fi))
              [ PhotoPhoto P.=. bs
              , PhotoMime P.=. fileContentType fi
              ]

          runDB $ insertMany_ (Social cid <$> links)
              
          addMessageI msgSuccess MsgNewRecordAdded
          redirect $ DataR CandidatesR
          
      FormSuccess ((r,Nothing),links) -> do
          cid <- runDB $ insert r
          runDB $ insertMany_ (Social cid <$> links)
          addMessageI msgSuccess MsgNewRecordAdded
          redirect $ DataR CandidatesR
          
      _otherwise -> defaultLayout $ do
          setTitleI MsgCandidate
          $(widgetFile "data/candidates/new")


getCandidatesR :: Handler Html
getCandidatesR = do
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
      
    candidates <- runDB $ select $ do
        x :& (_,nexams) <- from $ table @Candidate
            `leftJoin` from ( selectQuery $ do
                                  r <- from $ table @Exam
                                  groupBy (r ^. ExamCandidate)
                                  return (r ^. ExamCandidate, countRows :: SqlExpr (Value Int))
                            ) `on` (\(x :& (r,_)) -> just (x ^. CandidateId) ==. r)
        orderBy [desc (x ^. CandidateId)]
        return (x, coalesceDefault [nexams] (val 0))
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCandidates
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/candidates/main")
        $(widgetFile "data/candidates/candidates")


getCandidateNewR :: Handler Html
getCandidateNewR = do
    (fw,et) <- generateFormPost $ formCandidate [] Nothing
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "data/candidates/new")


formCandidate :: [Entity Social] -> Maybe (Entity Candidate) -> Form ((Candidate,Maybe FileInfo),[Text])
formCandidate medias candidate extra = do
    (fnameR,fnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFamilyName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateFamilyName . entityVal <$> candidate)
        
    (gnameR,gnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgGivenName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateGivenName . entityVal <$> candidate)
        
    (anameR,anameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgAdditionalName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateAdditionalName . entityVal <$> candidate)
        
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateBday . entityVal <$> candidate)
        
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateEmail . entityVal <$> candidate)
        
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidatePhone . entityVal <$> candidate)

    users <- liftHandler $ fieldListOptions <$> runDB ( select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), asc (x ^. UserId)]
        return ((x ^. UserName, x ^. UserEmail), x ^. UserId) )

    (userR,userV) <- mopt (selectFieldList users) FieldSettings
        { fsLabel = SomeMessage MsgUser
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (candidateUser . entityVal <$> candidate)
        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    linksRV <- forM (socialLink . entityVal <$> medias) $ \link -> mreq urlField FieldSettings
        { fsLabel = SomeMessage MsgSocialMedia
        , fsTooltip = Nothing, fsId = Nothing, fsName = Just nameSocialMediaLink
        , fsAttrs = []
        } (Just link)

    links <- S.toList . S.fromList . filter (not . T.null . T.strip) <$> lookupPostParams nameSocialMediaLink

    let r = (,) <$> ( (,) <$> (Candidate <$> fnameR <*> gnameR <*> anameR <*> bdayR <*> emailR <*> phoneR <*> userR)
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
           , $(widgetFile "data/candidates/form")
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



printf :: String -> Double -> String
printf = Printf.printf
