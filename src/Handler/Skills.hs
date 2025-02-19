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

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, delete
    , (^.), (%), (++.), (||.), (==.)
    , where_, like, val, upper_, just
    )
import Database.Persist
    ( Entity (Entity, entityVal), PersistStoreWrite (insert, replace)
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Form, Handler, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (AdminR)
    , AdminR
      ( SkillsR, SkillR, SkillEditFormR, SkillCreateFormR
      , SkillsSearchR, SkillsDeleteR
      )
    , AppMessage
      ( MsgSkills, MsgAdd, MsgSearch, MsgSkill, MsgSave
      , MsgCancel, MsgCode, MsgName, MsgDescr, MsgDelete
      , MsgDuplicateValue, MsgInvalidData, MsgBack
      , MsgNewRecordAdded, MsgRecordDeleted, MsgRecordEdited
      , MsgNoSkillsYet, MsgEdit, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgInvalidFormData
      )
    )

import Material3 (md3widget, md3textareaWidget)

import Model
    ( msgError, msgSuccess
    , SkillId, Skill (skillCode, skillName, Skill, skillDescr)
    , EntityField (SkillId, SkillCode, SkillName, SkillDescr)
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, whamlet
    , SomeMessage (SomeMessage), getMessages, addMessageI
    , getUrlRender, lookupPostParam, getUrlRenderParams
    , newIdent
    )

import Yesod.Core.Handler
    ( redirect, lookupGetParam, setUltDestCurrent
    )
import Yesod.Form
    ( FormResult (FormSuccess)
    , generateFormPost, runFormPost, Textarea (Textarea, unTextarea)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsAttrs, fsName)
    , textField, textareaField, mreq, mopt, unTextarea
    , checkM, runInputGet, iopt, intField, urlField
    )
import Yesod (YesodPersist(runDB))


postSkillR :: SkillId -> Handler Html
postSkillR sid = do
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x
    ((r,widget),enctype) <- runFormPost $ formSkill skill
    case r of
      FormSuccess x -> do
          runDB $ replace sid x
          addMessageI msgSuccess MsgRecordEdited
          redirect $ AdminR $ SkillR sid
      _ -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgSkill
          $(widgetFile "data/skills/edit")


getSkillEditFormR :: SkillId -> Handler Html
getSkillEditFormR sid = do
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x
    (widget,enctype) <- generateFormPost $ formSkill skill
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgSkill
        $(widgetFile "data/skills/edit")


getSkillR :: SkillId -> Handler Html
getSkillR sid = do
    location <- runInputGet $ iopt urlField "location"
    skill <- runDB $ selectOne $ do
        x <- from $ table @Skill
        where_ $ x ^. SkillId ==. val sid
        return x

    (fw0,et0) <- generateFormPost formSkillDelete
        
    let params = [("id", pack . show . fromSqlKey $ sid)]
    
    ult <- getUrlRenderParams >>= \rndr -> return $ fromMaybe (rndr (AdminR SkillsR) params) location
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSkill
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/skills/skill")


formSkillDelete :: Form ()
formSkillDelete extra = return (pure (), [whamlet|^{extra}|])


postSkillsDeleteR :: SkillId -> Handler Html
postSkillsDeleteR sid = do
    location <- getUrlRender >>= \rndr -> fromMaybe (rndr (AdminR SkillsR))  <$> lookupPostParam "location"
          
    ((fr,_),_) <- runFormPost formSkillDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Skill
              where_ $ x ^. SkillId ==. val sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect location
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect location


getSkillsSearchR :: Handler Html
getSkillsSearchR = do
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
        
    rndr <- getUrlRenderParams
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        idFormQuery <- newIdent
        $(widgetFile "data/skills/search")
        $(widgetFile "data/skills/skills")


getSkillCreateFormR :: Handler Html
getSkillCreateFormR = do
    (widget,enctype) <- generateFormPost $ formSkill Nothing
    msgs <- getMessages
    defaultLayout $ do
      setTitleI MsgSkill
      $(widgetFile "data/skills/create")


postSkillsR :: Handler Html
postSkillsR = do
    ((r,widget),enctype) <- runFormPost $ formSkill Nothing
    case r of
      FormSuccess skill -> do
          sid <- runDB $ insert skill
          addMessageI msgSuccess MsgNewRecordAdded
          redirect (AdminR SkillsR,[("id", pack . show . fromSqlKey $ sid)])
          
      _ -> defaultLayout $ do
          addMessageI msgError MsgInvalidData
          msgs <- getMessages
          setTitleI MsgSkill
          $(widgetFile "data/skills/create")


getSkillsR :: Handler Html
getSkillsR = do
    activated <- (toSqlKey <$>) <$> runInputGet (iopt intField "id")
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
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

    rndr <- getUrlRenderParams
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgSkills
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/skills/main")
        $(widgetFile "data/skills/skills") 


formSkill :: Maybe (Entity Skill) -> Form Skill
formSkill skill extra = do
    
    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (skillCode . entityVal <$> skill)
        
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
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
                      ^{md3widget v}

                    ^{md3textareaWidget descrV}
                    |]
    return (r,w)
  where
      uniqueCodeField = checkM uniqueCode textField

      uniqueCode :: Text -> Handler (Either AppMessage Text)
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
