{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (liftLoc, runLoggingT, LogLevel (LevelError))
import Control.Monad.Trans.Reader (ReaderT)

import Data.Default (def)
import qualified Data.Map as M (empty)

import Database.Esqueleto.Experimental (table, from, where_, delete, (^.))
import Database.Persist (insert_)
import Database.Persist.Sql ( runMigrationSilent, SqlBackend )
import Database.Persist.Sqlite
    ( createSqlitePoolWithConfig, runSqlPool, sqlDatabase
    )

import Foundation
    ( App (..), Handler, resourcesApp, unsafeHandler, getServiceWorkerR
    , Route (..)
    , DataR (..)
    , StatsR (..)
    )

import Language.Haskell.TH.Syntax (qLocation)

import Model
    ( migrateAll
    , AuthenticationType (UserAuthTypePassword), EntityField (UserSuper)
    , User
      ( User, userEmail, userPassword, userName, userSuper, userAdmin
      , userAuthType, userVerkey, userVerified
      )
    )

import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (Header)
import Network.Wai (Middleware, Application, mapResponseHeaders)
import qualified Network.Wai as W (Response)
import Network.Wai.Handler.Warp
    ( Settings, defaultSettings, defaultShouldDisplayException
    , runSettings, setHost, setOnException, setPort, getPort
    )
import Network.Wai.Middleware.Gzip
    ( gzip, GzipSettings (gzipFiles), GzipFiles (GzipCompress)
    )
import Network.Wai.Middleware.RequestLogger
    ( Destination (Logger), IPAddrSource (..), OutputFormat (..)
    , destination, mkRequestLogger, outputFormat
    )

import Settings
    ( AppSettings
      ( appSuperuser, appConnectionPoolConfig, appDatabaseConf, appMutableStatic
      , appStaticDir, appHost, appPort, appIpFromHeader, appDetailedRequestLogging
      )
    , Superuser (superuserUsername, superuserPassword)
    , configSettingsYmlValue
    )
    
import System.Environment.Blank (getEnv)
import System.Log.FastLogger
    ( defaultBufSize, newStdoutLoggerSet, toLogStr
    )

import Handler.Candidates
    ( getCandidatesR
    , getCandidateNewR
    , postCandidatesR, getCandidatePhotoR, getCandidateR
    , getCandidateEditFormR, postCandidateR
    , postCandidateDeleR, getCandidatesSearchR
    , getCandidateExamsR, getCandidateExamR
    , getCandidateSkillsR, getCandidatePhotosR
    )

import Handler.Common
    ( getWebAppManifestR, getSitemapR, getFaviconR, getRobotsR
    , getPhotoPlaceholderR
    )

import Handler.Stats
    ( getTopSkilledR, getSkilledR
    , getTopExamsR, getTopExamR
    , getExamSuccessRatesR
    , getTestSuccessRateR
    )

import Handler.Exams
    ( getExamsR, postExamsR
    , getExamR
    , getExamsLoginR
    , getExamsAfterLoginR
    , getExamEnrollmentFormR, postExamEnrollmentFormR
    , getExamUserEnrollmentR
    , getSearchExamsR
    , getSearchExamR
    )

import Handler.Steps
    ( getStepR, postStepR
    , getStepInvalidR
    , postCompleteR
    , getSummaryR
    , postCancelR
    , getRemainingTimeR
    , getWebSocketTimeoutR
    )

import Handler.Docs (getDocsR)

import Handler.Tests
    ( getSearchTestExamsR
    , getTestExamR
    , getTestSkillsR
    , getSearchTestExamR
    , getSearchTestExamSkillsR
    , getTestExamLoginR
    , getTestExamsR
    , getTestExamEnrollmentR
    , getTestExamUserEnrollmentR
    , getTestExamEnrollmentFormR, postTestExamEnrollmentFormR
    )
  
import Handler.Home
    ( getHomeR
    )


import Handler.Data.Remote
    ( getRemotesR
    , getRemoteR
    , getRemoteNewTestR, postRemoteNewTestR
    , getRemoteNewCandidatesR, postRemoteNewCandidatesR
    , getRemoteNewExamR, postRemoteNewExamR
    , getRemoteEditR
    , postRemoteDeleR
    )
    
import Handler.Data.Options
    ( getOptionsR, getOptionCreateFormR
    , postOptionsR, getOptionR, getOptionEditFormR
    , postOptionR, postOptionsDeleteR
    )
    
import Handler.Data.Stems
    ( getStemsR, getStemCreateFormR
    , postStemsR, getStemEditFormR
    , postStemR, postStemsDeleteR
    , getStemR
    )
    
import Handler.Data.Tests
    ( getTestsR, getTestCreateFormR, postTestsR
    , getTestEditFormR, postTestR, getTestR
    , postTestDeleR, getTestSearchR
    , postTestPublishR, postTestUnpublishR
    )

import Handler.Data.Users
    ( getUsersR, postUsersR
    , getUserR, postUserR
    , getUserNewR, getUserEditR, postUserDeleR
    , getUserPhotoR
    , getUserResetPasswordR, postUserResetPasswordR
    )
    
import Handler.Data.Skills
    ( getSkillsR, postSkillsR, getSkillCreateFormR
    , getSkillsSearchR, postSkillsDeleteR
    , getSkillR, getSkillEditFormR, postSkillR
    )

import Demo.DemoDataFR (populateFR)
import Demo.DemoDataRO (populateRO)
import Demo.DemoDataRU (populateRU)
import Demo.DemoDataEN (populateEN)
    
import Yesod.Auth (getAuth)
import Yesod.Auth.Email (saltPass)
import Yesod.Core (Yesod(messageLoggerSource))
import Yesod.Core.Dispatch (mkYesodDispatch, defaultMiddlewaresNoLogging, toWaiAppPlain)
import Yesod.Core.Types (Logger(loggerSet))
import Yesod.Default.Config2
    ( makeYesodLogger, getDevSettings, useEnv, loadYamlSettingsArgs
    , develMainHelper, loadYamlSettings, configSettingsYml
    )
import Yesod.Persist.Core (runDB)
import Yesod.Static (staticDevel, static)



-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.

mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    exams <- newTVarIO M.empty
    
    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePoolWithConfig
        (sqlDatabase $ appDatabaseConf appSettings)
        (appConnectionPoolConfig appSettings)

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc $ flip runSqlPool pool $ do
        void $ runMigrationSilent migrateAll

        delete $ do
            x <- from $ table @User
            where_ $ x ^. UserSuper
    
        superpass <- liftIO $ saltPass (superuserPassword . appSuperuser $ appSettings)
        insert_ User { userEmail = superuserUsername . appSuperuser $ appSettings
                     , userPassword = Just superpass
                     , userName = Just "Super User"
                     , userSuper = True
                     , userAdmin = True
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
                     
        demo <- liftIO $ getEnv "DEMO_LANG"
        case demo of
          Just "FR" -> populateFR
          Just "RO" -> populateRO
          Just "RU" -> populateRU
          Just _    -> populateEN
          Nothing   -> populateEN

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging $
        ( withHeader ("Service-Worker-Allowed","/") . gzip def { gzipFiles = GzipCompress }
        ) appPlain


withHeader :: Header -> Middleware
withHeader h app req res = app req $ res . addH h


addH :: Header -> W.Response -> W.Response
addH h = mapResponseHeaders (h :)



makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
