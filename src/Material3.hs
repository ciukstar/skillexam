{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Material3
  ( md3widget
  , md3textareaWidget
  , md3selectWidget
  , md3checkboxWidget
  , md3switchWidget
  , md3fileWidget
  , daytimeLocalField
  , md3radioField
  ) where

import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Time (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, WidgetFor, handlerToWidget)
import Yesod.Form.Fields
    ( FormMessage, datetimeLocalField, radioField', OptionList (olOptions)
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    )
import Yesod.Form.Types
    ( Field (fieldView)
    , FieldView (fvErrors, fvInput, fvLabel, fvRequired, fvId)
    )


md3radioField :: (RenderMessage m FormMessage, Eq a) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3radioField options = (radioField' options)
    { fieldView = \theId name attrs x isReq -> do
          opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget options
          let sel (Left _) _ = False
              sel (Right y) opt = optionInternalValue opt == y
          [whamlet|
<div ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    <label.radio for=#{theId}-#{i}>
      <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt} :sel x opt:checked>
      <span style="white-space:normal">
        #{optionDisplay opt}
    
|] }


md3fileWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3fileWidget v = [whamlet|
  <button.transparent.border.small>
    <i>upload_file
    <span>#{fvLabel v}
    ^{fvInput v}

  $maybe err <- fvErrors v
    <span.error-text>#{err}
|]


md3switchWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3switchWidget v = [whamlet|
  <div.field.no-margin.middle-align.small :isJust (fvErrors v):.invalid>
    <nav.no-padding>          
      <label.switch>
        ^{fvInput v}
        <span style="padding-left:1rem">
          #{fvLabel v}

      $maybe err <- fvErrors v
        <span.error>#{err}
|]


md3checkboxWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3checkboxWidget v = [whamlet|
  <label.checkbox.small>
    ^{fvInput v}
    <span>#{fvLabel v}
|]

    
md3textareaWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3textareaWidget v = [whamlet|
  <div.field.border.round.label.textarea.small :isJust (fvErrors v):.invalid>
    ^{fvInput v}
    <label for=#{fvId v}>
      #{fvLabel v}
      $if fvRequired v
        <sup>*
    $maybe err <- fvErrors v
      <span.error>#{err}
|]

    
md3selectWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3selectWidget v = [whamlet|
  <div.field.label.suffix.border.round.small :isJust (fvErrors v):.invalid>
    ^{fvInput v}
    <label for=#{fvId v}>
      #{fvLabel v}
      $if fvRequired v
        <sup>*
    <i>arrow_drop_down
    $maybe err <- fvErrors v
      <span.error>#{err}
|]


md3widget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widget v = [whamlet|
  <div.field.label.border.round.small :isJust (fvErrors v):.invalid>

    ^{fvInput v}
    <label for=#{fvId v}>
      #{fvLabel v}
      $if fvRequired v
        <sup>*

    $maybe err <- fvErrors v
      <span.error>#{err}
|]


daytimeLocalField :: RenderMessage m FormMessage => Field (HandlerFor m) LocalTime
daytimeLocalField = datetimeLocalField { fieldView = \theId name attrs ex req -> [whamlet|
<input ##{theId} type=datetime-local name=#{name} :req:required value=#{either id showVal ex} *{attrs}>
|] }
  where
      showVal = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
