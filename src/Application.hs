{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Control.Monad (void)

import Data.Bool (Bool (False))
import Data.Maybe (Maybe (Nothing))

import Import


import Network.Wai.Middleware.Gzip
    ( gzip, GzipSettings (gzipFiles), GzipFiles (GzipCompress)
    )
import Control.Monad.Logger (liftLoc, runLoggingT)

import Database.Persist (insert_)
import Database.Persist.Sql ( runMigrationSilent )
import Database.Persist.Sqlite
    ( createSqlitePoolWithConfig, runSqlPool, sqlDatabase
    )
import Language.Haskell.TH.Syntax (qLocation)

import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
    ( Settings, defaultSettings, defaultShouldDisplayException,
      runSettings, setHost, setOnException, setPort, getPort
    )
import Network.Wai.Middleware.RequestLogger
    ( Destination (Logger), IPAddrSource (..), OutputFormat (..)
    , destination, mkRequestLogger, outputFormat
    )
       
import System.Environment.Blank (getEnv)
import System.Log.FastLogger
    ( defaultBufSize, newStdoutLoggerSet, toLogStr
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
    ( getStepR
    , postStepR
    , postCompleteR
    , getSummaryR
    , postTerminateR
    , getRemainingTimeR
    )

import Handler.Candidates
    ( getCandidatesR, getCandidateCreateFormR
    , postCandidatesR, getCandidatePhotoR, getCandidateR
    , getCandidateEditFormR, postCandidateR
    , postCandidateDeleR, getCandidatesSearchR
    , getCandidateExamsR, getCandidateExamR
    , getCandidateSkillsR, getCandidatePhotosR
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

import Handler.Common ( getFaviconR, getRobotsR, getPhotoPlaceholderR )

import Demo.DemoDataFR (populateFR)
import Demo.DemoDataRO (populateRO)
import Demo.DemoDataRU (populateRU)
import Demo.DemoDataEN (populateEN)

import Yesod.Auth.Email (saltPass)


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

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePoolWithConfig
        (sqlDatabase $ appDatabaseConf appSettings)
        (appConnectionPoolConfig appSettings)

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc $ flip runSqlPool pool $ do
        void $ runMigrationSilent migrateAll
    
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
    return $ logWare $ defaultMiddlewaresNoLogging $ gzip def { gzipFiles = GzipCompress } appPlain

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
