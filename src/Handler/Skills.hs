{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Skills
  ( getSkillsR
  , postSkillsR
  , getSkillCreateFormR
  , getSkillsSearchR
  , postSkillsDeleteR
  , getSkillR
  , getSkillEditFormR
  , postSkillR
  ) where

import Data.Text (Text, unpack, pack)
import Data.Maybe (fromMaybe, isJust)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, WidgetFor, whamlet
    , SomeMessage (SomeMessage), getMessages, addMessageI
    , lookupPostParams, getUrlRender, lookupPostParam, getUrlRenderParams, getCurrentRoute
    )
import Settings (widgetFile)

import Yesod.Core.Handler
    ( HandlerFor, redirect, lookupGetParam
    , setUltDestCurrent, lookupSession
    )

import Foundation
    ( App
    , Route
      ( AdminR
      , SignInR, SignOutR
      , PhotoPlaceholderR
      )
    , AdminR
      ( SkillsR, SkillR, CandidatePhotoR, SkillEditFormR, SkillCreateFormR
      , SkillsSearchR, SkillsDeleteR
      )
    , AppMessage
      ( MsgSkills, MsgAdd, MsgSearch, MsgSkill, MsgSave
      , MsgCancel, MsgCode, MsgName, MsgDescr, MsgDelete
      , MsgClose, MsgPleaseConfirm, MsgAreYouSureDelete
      , MsgDuplicateValue, MsgInvalidData
      , MsgLogin, MsgLogout, MsgPhoto, MsgNewRecordAdded
      , MsgRecordDeleted, MsgRecordEdited, MsgNoSkillsYet
      )
    )

import Model
    ( SkillId, Skill (skillCode, skillName, Skill, skillDescr)
    , EntityField (SkillId, SkillCode, SkillName, SkillDescr, CandidateId)
    , Candidate (Candidate), userSessKey
    )

import Yesod.Form
    ( MForm, FormResult (FormSuccess)
    , FieldView (fvInput, fvId, fvLabel, fvErrors)
    , generateFormPost, runFormPost, Textarea (Textarea, unTextarea)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsAttrs, fsName)
    , textField, textareaField, mreq, mopt, unTextarea
    , checkM, runInputGet, iopt, intField, urlField
    )

import Yesod (YesodPersist(runDB))

import Database.Persist
    ( Entity (Entity, entityVal), PersistStoreWrite (insert, replace)
    )

import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, delete
    , (^.), (%), (++.), (||.), (==.)
    , where_, like, val, upper_, in_, valList, just
    )


postSkillR :: SkillId -> HandlerFor App Html
postSkillR sid = do
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x
    ((r,widget),enctype) <- runFormPost $ formSkill skill
    case r of
      FormSuccess x -> do
          runDB $ replace sid x
          addMessageI "--mdc-theme-info" MsgRecordEdited
          redirect $ AdminR $ SkillR sid
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgSkill
          $(widgetFile "skills/edit")


getSkillEditFormR :: SkillId -> HandlerFor App Html
getSkillEditFormR sid = do
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x
    (widget,enctype) <- generateFormPost $ formSkill skill
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgSkill
        $(widgetFile "skills/edit")


getSkillR :: SkillId -> HandlerFor App Html
getSkillR sid = do
    location <- runInputGet $ iopt urlField "location"
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x
    let params = [("id", pack . show . fromSqlKey $ sid)]
    
    ult <- getUrlRenderParams >>= \rndr -> return $ fromMaybe (rndr (AdminR SkillsR) params) location
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSkill
        $(widgetFile "skills/skill")


postSkillsDeleteR :: HandlerFor App Html
postSkillsDeleteR = do
    ids <- (toSqlKey . read . unpack <$>) <$> lookupPostParams "id"
    location <- getUrlRender >>= \rndr -> fromMaybe (rndr (AdminR SkillsR))  <$> lookupPostParam "location"
    runDB $ delete $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId `in_` valList ids
    addMessageI "--mdc-theme-info" MsgRecordDeleted
    redirect location


getSkillsSearchR :: HandlerFor App Html
getSkillsSearchR = do
    curr <- fromMaybe (AdminR SkillsR) <$> getCurrentRoute
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    scrollY <- fromMaybe "0" <$> lookupGetParam "scrollY"
    mq <- lookupGetParam "q"
    skills <- runDB $ select $ do
        x <- from $ table @Skill
        case mq of
          Nothing -> return ()
          Just q -> where_ $ (upper_ (x ^. SkillCode) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. SkillName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. SkillDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%))
        orderBy [desc (x ^. SkillId)]
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        $(widgetFile "skills/search")
        $(widgetFile "skills/skills")


getSkillCreateFormR :: HandlerFor App Html
getSkillCreateFormR = do
    (widget,enctype) <- generateFormPost $ formSkill Nothing
    msgs <- getMessages
    defaultLayout $ do
      setTitleI MsgSkill
      $(widgetFile "skills/create")


postSkillsR :: HandlerFor App Html
postSkillsR = do
    ((r,widget),enctype) <- runFormPost $ formSkill Nothing
    case r of
      FormSuccess skill -> do
          sid <- runDB $ insert skill
          addMessageI "--mdc-theme-info" MsgNewRecordAdded
          redirect (AdminR SkillsR,[("id", pack . show . fromSqlKey $ sid)])
      _ -> defaultLayout $ do
          addMessageI "--mdc-theme-error" MsgInvalidData
          msgs <- getMessages
          setTitleI MsgSkill
          $(widgetFile "skills/create")


getSkillsR :: HandlerFor App Html
getSkillsR = do
    curr <- fromMaybe (AdminR SkillsR) <$> getCurrentRoute
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    mcid <- (toSqlKey . read . unpack <$>) <$> lookupSession userSessKey
    candidate <- case mcid of
      Just cid -> runDB $ selectOne $ do
          x <- from $ table @Candidate
          where_ $ x ^. CandidateId ==. val cid
          return x
      _ -> return Nothing
    mq <- lookupGetParam "q"
    skills <- runDB $ select $ do
        x <- from $ table @Skill
        case mq of
          Nothing -> return ()
          Just q -> where_ $ (upper_ (x ^. SkillCode) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. SkillName) `like` (%) ++. upper_ (val q) ++. (%))
            ||. (upper_ (x ^. SkillDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%))
        orderBy [desc (x ^. SkillId)]
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgSkills
        $(widgetFile "skills/main")
        $(widgetFile "skills/skills")


formSkill :: Maybe (Entity Skill)
          -> Html -> MForm (HandlerFor App) (FormResult Skill, WidgetFor App ())
formSkill skill extra = do
    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (skillCode . entityVal <$> skill)
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (skillName . entityVal <$> skill)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescr
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (skillDescr . entityVal <$> skill)

    let r = Skill <$> codeR <*> nameR <*> descrR
    let w = [whamlet|
#{extra}
$forall v <- [codeV,nameV]
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors v):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label #floatingLabel#{fvId v}>#{fvLabel v}
    ^{fvInput v}
    <span.mdc-line-ripple>
  <div.mdc-text-field-helper-line>
    <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
      $maybe errs <- fvErrors v
        #{errs}

<label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label #floatingLabel#{fvId descrV}>#{fvLabel descrV}
  <span.mdc-text-field__resizer>
    ^{fvInput descrV}
  <span.mdc-line-ripple>
<div.mdc-text-field-helper-line>
  <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
    $maybe errs <- fvErrors descrV
      #{errs}
|]
    return (r,w)
  where
      uniqueCodeField = checkM uniqueCode textField

      uniqueCode :: Text -> HandlerFor App (Either AppMessage Text)
      uniqueCode code = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Skill
              where_ $ x ^. SkillCode ==. val code
              return x
          return $ case mx of
            Nothing -> Right code
            Just (Entity sid _) -> case skill of
              Nothing -> Left MsgDuplicateValue
              Just (Entity sid' _) | sid' == sid -> Right code
                                   | otherwise -> Left MsgDuplicateValue
