{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common
  ( getWebAppManifestR
  , getSitemapR
  , getPhotoPlaceholderR
  , getFaviconR
  , getRobotsR
  ) where

import Conduit (yield)

import Data.Aeson (Value (String), (.=), object)
import Data.FileEmbed (embedFile)

import Foundation
    ( App, Handler
    , Route (HomeR, DocsR, StaticR)
    , AppMessage
      ( MsgAppName, MsgMetaDescription, MsgMainMenu, MsgWelcome
      , MsgMyExams
      )
    )

import Settings.StaticFiles
    ( img_logo_144x144_svg
    , img_screenshot_1_narrow_png, img_screenshot_1_wide_png
    , img_screenshot_2_narrow_png, img_screenshot_2_wide_png
    , img_screenshot_3_narrow_png, img_screenshot_3_wide_png
    )

import Yesod.Core
    ( TypedContent (TypedContent), ToContent (toContent), typePlain
    , typeSvg, getUrlRender, getMessageRender, selectRep, provideJson, array
    )
import Yesod.Core.Handler (HandlerFor, cacheSeconds)
import Yesod.Sitemap (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly))

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    urlr <- getUrlRender
    msgr <- getMessageRender
    selectRep $ provideJson $ object
        [ "name" .= msgr MsgAppName
        , "short_name" .= msgr MsgAppName
        , "description" .= msgr MsgMetaDescription
        , "categories" .= array [String "social"]
        , "start_url" .= urlr HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array
          [ object [ "src" .= urlr (StaticR img_logo_144x144_svg)
                   , "type" .= String "image/svg+xml"
                   , "sizes" .= String "144x144"
                   , "purpose" .= String "any"
                   ]
          , object [ "src" .= urlr (StaticR img_logo_144x144_svg)
                   , "type" .= String "image/svg+xml"
                   , "sizes" .= String "144x144"
                   , "purpose" .= String "maskable"
                   ]
          ]
        , "screenshots" .= array
          [ object [ "src" .= urlr (StaticR img_screenshot_1_narrow_png)
                   , "sizes" .= String "450x800"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "narrow"
                   , "label" .= msgr MsgMainMenu
                   ]
          , object [ "src" .= urlr (StaticR img_screenshot_1_wide_png)
                   , "sizes" .= String "1920x926"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "wide"
                   , "label" .= msgr MsgMainMenu
                   ]
          , object [ "src" .= urlr (StaticR img_screenshot_2_narrow_png)
                   , "sizes" .= String "450x800"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "narrow"
                   , "label" .= msgr MsgWelcome
                   ]
          , object [ "src" .= urlr (StaticR img_screenshot_2_wide_png)
                   , "sizes" .= String "1920x926"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "wide"
                   , "label" .= msgr MsgWelcome
                   ]
          , object [ "src" .= urlr (StaticR img_screenshot_3_narrow_png)
                   , "sizes" .= String "450x800"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "narrow"
                   , "label" .= msgr MsgMyExams
                   ]
          , object [ "src" .= urlr (StaticR img_screenshot_3_wide_png)
                   , "sizes" .= String "1920x926"
                   , "type" .= String "image/png"
                   , "form_factor" .= String "wide"
                   , "label" .= msgr MsgMyExams
                   ]
          ]
        ]


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl DocsR Nothing (Just Monthly) (Just 1.0)


getPhotoPlaceholderR :: Handler TypedContent
getPhotoPlaceholderR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                          return $ TypedContent typeSvg
                                 $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg")

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: HandlerFor App TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
