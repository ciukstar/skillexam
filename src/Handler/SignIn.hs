{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.SignIn
  ( getSignInR
  , postSignInR
  , postSignOutR
  ) where

import Data.Text (unpack, pack)
import Data.Maybe (isJust, fromMaybe)
import Data.Bifunctor (Bifunctor(first))
import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, SomeMessage (SomeMessage)
    , setSession, deleteSession, redirectUltDest, getUrlRender, lookupSession
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Fields (hiddenField)

import Foundation
    ( Handler, Widget
    , Route (AdminR, HomeR, SignInR, PhotoPlaceholderR)
    , AdminR (CandidatePhotoR, CandidatePhotosR)
    , AppMessage
      ( MsgLogin, MsgCandidate, MsgPhoto, MsgSignIn
      , MsgAuthentication
      )
    )

import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvErrors)
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc
    , (^.)
    )
    
import Model
    ( userSessKey, keyUtlDest
    , Candidate(Candidate), CandidateId
    , EntityField
      ( CandidateFamilyName, CandidateGivenName, CandidateAdditionalName
      )
    )


postSignOutR :: Handler Html
postSignOutR = do
    deleteSession userSessKey
    redirectUltDest HomeR


postSignInR :: Handler Html
postSignInR = do
    candidates <- runDB $ select $ do
        x <- from $ table @Candidate
        orderBy [ asc (x ^. CandidateFamilyName)
                , asc (x ^. CandidateGivenName)
                , asc (x ^. CandidateAdditionalName)
                ]
        return x
    ((fr,widget),enctype) <- runFormPost $ formSignIn candidates
    case fr of
      FormSuccess r -> do
          setSession userSessKey (pack . show . fromSqlKey $ r)
          redirectUltDest HomeR
      _ -> defaultLayout $ do
          ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
          setTitleI MsgSignIn
          $(widgetFile "sign-in/sign-in")


getSignInR :: Handler Html
getSignInR = do
    candidates <- runDB $ select $ do
        x <- from $ table @Candidate
        orderBy [ asc (x ^. CandidateFamilyName)
                , asc (x ^. CandidateGivenName)
                , asc (x ^. CandidateAdditionalName)
                ]
        return x
    (widget,enctype) <- generateFormPost $ formSignIn candidates
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
    defaultLayout $ do
        setTitleI MsgLogin
        $(widgetFile "sign-in/sign-in")


formSignIn :: [Entity Candidate] -> Html -> MForm Handler (FormResult CandidateId, Widget)
formSignIn candidates extra = do
    (cidR,cidV) <- first (toSqlKey . read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgCandidate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing

    let r = cidR
    let w = [whamlet|
#{extra}
<div.mdc-select.mdc-select--filled.mdc-select--required
  :isJust (fvErrors cidV):.mdc-select--invalid data-mdc-auto-init=MDCSelect>

  ^{fvInput cidV}
  
  <div.mdc-select__anchor role=button aria-haspopup=listbox aria-expanded=false>
    <span.mdc-select__ripple>
    <span.mdc-floating-label>#{fvLabel cidV}
    <span.mdc-select__selected-text-container>
      <span.mdc-select__selected-text>
    <span.mdc-select__dropdown-icon>
      <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
        <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
        <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
    <span.mdc-line-ripple>
    
  <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
    <ul.mdc-deprecated-list.mdc-deprecated-list--avatar-list role=listbox>
      $forall Entity cid (Candidate fname gname aname _) <- candidates
        <li.mdc-deprecated-list-item data-value=#{fromSqlKey cid}>
          <span.mdc-deprecated-list-item__ripple>
          <span.mdc-deprecated-list-item__graphic>
            <img.photo src=@{AdminR $ CandidatePhotoR cid} alt=_{MsgPhoto} width=40 heigt=40
              onerror="this.src = '@{PhotoPlaceholderR}'">
          <span.mdc-deprecated-list-item__text>
            #{fname} #{gname}
            $maybe aname <- aname
              \ #{aname}

$maybe errs <- fvErrors cidV
  <div.mdc-select-helper-text.mdc-select-helper-text--validation-msg>
    #{errs}
|]
    return (r,w)
    
