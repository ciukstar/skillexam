{-# LANGUAGE TemplateHaskell #-}

module Handler.Docs (getDocsR) where

import Foundation
    ( Handler, widgetAccount, widgetMainMenu
    , Route (AdminR, HomeR, MyExamsR, StaticR)
    , AdminR (SkillsR, TestsR, CandidatesR)
    , AppMessage
      ( MsgAppName, MsgDocs, MsgCandidate, MsgSkill, MsgBasicEntities
      , MsgOverview, MsgAnswer, MsgExam, MsgOption, MsgQuestion
      , MsgExam, MsgDiagramERD, MsgTest
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010, MsgDoc011, MsgDoc012
      , MsgDoc013, MsgDoc014, MsgDoc015, MsgDoc016, MsgDoc017, MsgDoc018
      , MsgDoc019, MsgDoc020, MsgDoc021, MsgDoc022, MsgDoc023, MsgDoc024
      , MsgDoc025, MsgDoc026
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_SkillExam_ERD_svg)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getUrlRender, preEscapedToMarkup
    , RenderMessage (renderMessage), getYesod, languages, newIdent
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    app <- getYesod
    langs <- languages
    rndr <- getUrlRender
    defaultLayout $ do
        setTitleI MsgDocs
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "docs/docs")
