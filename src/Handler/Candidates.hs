{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Candidates
  ( getCandidatesR
  , getCandidateCreateFormR
  , postCandidateR
  , postCandidatesR
  , getCandidatePhotoR
  , getCandidateR
  , getCandidateEditFormR
  , postCandidatesDeleteR
  , getCandidatesSearchR
  , getCandidateExamsR
  , getCandidateSkillsR
  , getCandidatePhotosR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.List.Safe as LS (head)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)
import Data.Text (unpack)
import Data.Text.ICU.Calendar (setDay)
import Data.Text.ICU
    ( LocaleName(Locale), calendar, CalendarType (TraditionalCalendarType)
    , standardDateFormatter, FormatStyle (NoFormatStyle, ShortFormatStyle)
    , formatCalendar
    )
import Text.Hamlet (Html)
import Text.Julius (rawJS)

import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    (MForm, FormResult (FormSuccess)
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (fsLabel, FieldSettings, fsTooltip, fsId, fsName, fsAttrs)
    )

import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost)
import Yesod.Form.Fields (textField, dayField, fileField, intField)

import Yesod.Core.Handler
    ( HandlerFor, redirect, FileInfo, fileSourceByteString, languages
    , getMessages, addMessageI, lookupPostParams, setUltDestCurrent
    , redirectUltDest, lookupSession, getUrlRender, lookupPostParam
    , getCurrentRoute, deleteSession
    )
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), setTitleI
    , TypedContent (TypedContent), ToContent (toContent), emptyContent
    , liftIO
    )
import Yesod.Core.Widget (WidgetFor, whamlet)
import Settings (widgetFile)

import Foundation
    ( App
    , Route
      ( PhotoPlaceholderR, MyExamR
      , SignInR, SignOutR, AdminR
      )
    , AdminR
      ( CandidatesR, CandidateR, CandidateSkillsR, CandidatePhotoR, CandidateExamsR
      , CandidatesDeleteR, CandidatesSearchR, CandidateCreateFormR
      , CandidateEditFormR
      )
    , AppMessage
      ( MsgCandidates, MsgSearch, MsgAdd, MsgFamilyName, MsgGivenName, MsgAdditionalName
      , MsgCandidate, MsgCancel, MsgSave, MsgBirthday, MsgPhoto, MsgSkills
      , MsgDelete, MsgDetails, MsgBack, MsgAge, MsgExams
      , MsgInvalidData, MsgClose, MsgPleaseConfirm, MsgAreYouSureDelete
      , MsgLogin, MsgLogout, MsgNoCandidatesYet
      , MsgNewRecordAdded, MsgRecordEdited, MsgRecordDeleted
      , MsgPass, MsgFail, MsgNoSkillsYet, MsgNoExamsYet, MsgPassExamInvite
      )
    )

import Database.Persist
    ( Entity (Entity, entityVal), (=.)
    , PersistStoreWrite (insert, replace), PersistUniqueWrite (upsert)
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, innerJoin, on
    , (^.), (==.), (%), (++.), (||.), (:&) ((:&)), (<=.), (&&.)
    , val, selectOne, where_, delete, in_, valList
    , upper_, like, just, Value (Value)
    , distinct, selectQuery, groupBy, countRows, leftJoin, coalesceDefault
    , SqlExpr, sum_, subSelectList
    )

import Model
    ( Candidate
      ( Candidate, candidateFamilyName, candidateGivenName
      , candidateAdditionalName, candidateBday
      )
    , EntityField
      ( CandidateId, PhotoCandidate, PhotoPhoto, CandidateFamilyName
      , CandidateGivenName, CandidateAdditionalName
      , ExamTest, ExamCandidate
      , StemId, TestId, ExamId, AnswerExam
      , OptionId, AnswerOption, OptionStem, StemSkill, SkillId, OptionKey
      , SkillName, OptionPoints, TestPass, StemTest
      )
    , CandidateId, Photo (Photo), ultDestKey
    , Exam (Exam)
    , Test (Test), Stem, userSessKey, Answer, Option, Skill (Skill)
    )


getCandidatesSearchR :: HandlerFor App Html
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
        $(widgetFile "candidates/search")
        $(widgetFile "candidates/candidates")


postCandidatesDeleteR :: HandlerFor App Html
postCandidatesDeleteR = do
    uid <-  (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
    cids <- (toSqlKey . read . unpack <$>) <$> lookupPostParams "id"
    runDB $ delete $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId `in_` valList cids

    when (uid `elem` (pure <$> cids)) $ deleteSession userSessKey

    location <- getUrlRender >>= \rndr -> fromMaybe (rndr $ AdminR CandidatesR) <$> do
        l <- lookupPostParam "location"
        ult <- lookupSession ultDestKey
        return $ l <|> ult
    addMessageI "--mdc-theme-info" MsgRecordDeleted
    redirect location


postCandidateR :: CandidateId -> HandlerFor App Html
postCandidateR cid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
    ((fr,widget),enctype) <- runFormPost $ formCandidate candidate
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr $ AdminR CandidatesR) <$> lookupSession ultDestKey
    case fr of
      FormSuccess (c,photo) -> do
          runDB $ replace cid c
          case photo of
            Just fs -> do
                bs <- fileSourceByteString fs
                _ <- runDB $ upsert (Photo cid bs "image/avif") [PhotoPhoto =. bs]
                return ()
            _ -> return ()
          addMessageI "--mdc-theme-info" MsgRecordEdited
          redirectUltDest $ AdminR CandidatesR
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgCandidate
          $(widgetFile "candidates/edit")


getCandidateEditFormR :: CandidateId -> HandlerFor App Html
getCandidateEditFormR cid = do
    candidate <- runDB $ selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x
    (widget,enctype) <- generateFormPost $ formCandidate candidate
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr $ AdminR CandidatesR) <$> lookupSession ultDestKey
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgCandidate
        $(widgetFile "candidates/edit")


getCandidateSkillsR :: CandidateId -> HandlerFor App Html
getCandidateSkillsR cid = do
    curr <- getCurrentRoute
    candidate <- ((,Nothing) <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x )
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
    let tab = $(widgetFile "candidates/skills")
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "candidates/candidate")


getCandidateExamsR :: CandidateId -> HandlerFor App Html
getCandidateExamsR cid = do
    curr <- getCurrentRoute
    candidate <- ((,Nothing) <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Candidate
        where_ $ x ^. CandidateId ==. val cid
        return x )
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
    let tab = $(widgetFile "candidates/exams")
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "candidates/candidate")


getCandidateR :: CandidateId -> HandlerFor App Html
getCandidateR cid = do
    curr <- getCurrentRoute
    candidate <- do
        candidate <- runDB $ selectOne $ do
            x <- from $ table @Candidate
            where_ $ x ^. CandidateId ==. val cid
            return x
        y <- liftIO $ (\(y,_,_) -> y) . toGregorian .  utctDay <$> getCurrentTime
        return $ candidate >>= \c -> return (c, (y -) . (\(y',_,_) -> y') . toGregorian <$> (candidateBday . entityVal) c)

    loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
    cal <- liftIO $ calendar "GMT+0300" loc TraditionalCalendarType
    fmtDay <- liftIO $ standardDateFormatter NoFormatStyle ShortFormatStyle loc "GMT+0300"
    let tab = $(widgetFile "candidates/details")
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgCandidate
        $(widgetFile "candidates/candidate")


getCandidatePhotosR :: HandlerFor App TypedContent
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


getCandidatePhotoR :: CandidateId -> HandlerFor App TypedContent
getCandidatePhotoR cid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Photo
        where_ $ x ^. PhotoCandidate ==. val cid
        return x
    return $ case photo of
      Just (Entity _ (Photo _ bs _)) -> TypedContent "image/avif" $ toContent bs
      Nothing -> TypedContent "image/avif" emptyContent


postCandidatesR :: HandlerFor App Html
postCandidatesR = do
    ((fr,widget),enctype) <- runFormPost $ formCandidate Nothing
    case fr of
      FormSuccess (x,photo) -> do
          cid <- runDB $ insert x
          case photo of
            Just fs -> do
                bs <- fileSourceByteString fs
                _ <- runDB $ upsert (Photo cid bs "image/avif") [PhotoPhoto =. bs]
                return ()
            Nothing -> return ()
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirectUltDest $ AdminR CandidatesR
      _ -> defaultLayout $ do
          ult <- getUrlRender >>= \rndr -> fromMaybe (rndr $ AdminR CandidatesR) <$> lookupSession ultDestKey
          setTitleI MsgCandidate
          $(widgetFile "candidates/create")


getCandidatesR :: HandlerFor App Html
getCandidatesR = do
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    mcid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
    candidate <- case mcid of
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Candidate
          where_ $ x ^. CandidateId ==. val cid
          return x
      _ -> return Nothing
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
        $(widgetFile "candidates/main")
        $(widgetFile "candidates/candidates")


getCandidateCreateFormR :: HandlerFor App Html
getCandidateCreateFormR = do
    (widget,enctype) <- generateFormPost $ formCandidate Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr $ AdminR CandidatesR) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgCandidate
        $(widgetFile "candidates/create")


formCandidate :: Maybe (Entity Candidate)
              -> Html -> MForm (HandlerFor App) (FormResult (Candidate,Maybe FileInfo), WidgetFor App ())
formCandidate candidate extra = do
    (fnameR,fnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgFamilyName
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (candidateFamilyName . entityVal <$> candidate)
    (gnameR,gnameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgGivenName
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (candidateGivenName . entityVal <$> candidate)
    (anameR,anameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgAdditionalName
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (candidateAdditionalName . entityVal <$> candidate)
    (bdayR,bdayV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (candidateBday . entityVal <$> candidate)
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("style","display:none"),("onchange","displayPhoto(this)")]
        } Nothing

    let r = (,) <$> (Candidate <$> fnameR <*> gnameR <*> anameR <*> bdayR) <*> photoR
    let w = [whamlet|
#{extra}
^{fvInput photoV}
<ul.mdc-image-list>
  <li.mdc-image-list__item onclick="document.querySelector('##{fvId photoV}').click()">
    <div.mdc-image-list__image-aspect-container>
      $maybe Entity cid _ <- candidate
        <img.mdc-image-list__image #photo src=@{AdminR $ CandidatePhotoR cid} alt=_{MsgPhoto}
          onerror="this.src='@{PhotoPlaceholderR}'">
      $nothing
        <img.mdc-image-list__image #photo src=@{PhotoPlaceholderR} alt=_{MsgPhoto}>
    <div.mdc-image-list__supporting>
      <span.mdc-image-list__label>_{MsgPhoto}
$forall v <- [fnameV,gnameV,anameV,bdayV]
  <label.mdc-text-field.mdc-text-field--filled.form-field data-mdc-auto-init=MDCTextField
    :isJust (fvErrors v):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel v}
    ^{fvInput v}
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors v
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg>
        #{errs}
|]
    return (r,w)
