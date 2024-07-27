{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Foundation where

import Import.NoFoundation
  ( ($)
  , Eq((==))
  , Monad(return)
  , Bool(True)
  , IO
  , Either
  , widgetFile
  , AppSettings
    ( appShouldLogAll
    , appAuthDummyLogin
    , appRoot
    , appAnalytics
    , appStaticDir
    )
  , Text
  , Route(StaticRoute, LoginR)
  , Static
  , User(User, userPassword, userIdent)
  , UserId
  , Unique(UniqueUser)
  , SqlBackend
  , (<$>)
  , flip
  , (||)
  , (.)
  , (++)
  , defaultClientSessionBackend
  , defaultYesodMiddleware
  , getApprootText
  , guessApproot
  , widgetToPageContent
  , mkYesodData
  , addScript
  , addStylesheet
  , parseRoutesFile
  , defaultFormMessage
  , defaultGetDBRunner
  , base64md5
  , LByteString
  , Html
  , HasHttpManager(..)
  , Manager
  , LogLevel(LevelError, LevelWarn)
  , Entity(Entity)
  , PersistStoreWrite(insert)
  , PersistUniqueRead(getBy)
  , SqlPersistT
  , Lang
  , RenderMessage(..)
  , MonadHandler (liftHandler, HandlerSite)
  , Yesod (approot, makeLogger, shouldLogIO, addStaticContent, isAuthorized
          , authRoute, defaultLayout, yesodMiddleware, makeSessionBackend
          )
  , ToTypedContent
  , Approot(ApprootRequest)
  , AuthResult(Authorized, Unauthorized)
  , PageContent(pageBody, pageTitle, pageHead)
  , SessionBackend
  , RenderRoute(Route, renderRoute)
  , FormMessage
  , DBRunner
  , YesodPersist(..)
  , YesodPersistRunner(..)
  , getAuth
  , AuthPlugin
  , AuthenticationResult(Authenticated)
  , Creds(credsIdent)
  , YesodAuth
    ( authPlugins
    , authenticate
    , redirectToReferer
    , logoutDest
    , loginDest
    , AuthId, maybeAuthId
    )
  , YesodAuthPersist
  , Auth
  , getCurrentRoute
  )

import Settings.StaticFiles
    ( material_components_web_min_css
    , material_components_web_min_js
    , js_cookie_3_0_5_min_js
    , css_outlined_css
    )

import Data.Maybe (Maybe (..), fromMaybe)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Yesod.Core.Handler
    ( getYesod, defaultCsrfCookieName, defaultCsrfHeaderName
    , withUrlRenderer, languages
    )
-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy ( authDummy )

import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Text.Shakespeare.I18N (mkMessage)
import qualified Data.List.Safe as LS (head)

import Model (Skills, ExamId, CandidateId, OptionId, SkillId, TestId, StemId)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }


mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
      master <- getYesod
      currRoute <- getCurrentRoute
      
      pc <- widgetToPageContent $ do
          addStylesheet $ StaticR css_outlined_css
          addStylesheet $ StaticR material_components_web_min_css
          addScript $ StaticR material_components_web_min_js
          $(widgetFile "default-layout")
      
      langs <- languages
      let lang = fromMaybe "en" . LS.head $ langs 
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (AdminR SkillsR) _ = return Authorized
    isAuthorized (AdminR TestsR) _ = return Authorized
    isAuthorized (AdminR CandidatesR) _ = return Authorized
    isAuthorized (AdminR SkillCreateFormR) _ = return Authorized
    isAuthorized (AdminR SkillsSearchR) _ = return Authorized
    isAuthorized (AdminR SkillsDeleteR) _ = return Authorized
    isAuthorized (AdminR (SkillR _)) _ = return Authorized
    isAuthorized (AdminR (SkillEditFormR _)) _ = return Authorized
    isAuthorized (AdminR TestCreateFormR) _ = return Authorized
    isAuthorized (AdminR TestEditFormR {}) _ = return Authorized
    isAuthorized (AdminR TestR {}) _ = return Authorized
    isAuthorized (AdminR TestsDeleteR) _ = return Authorized
    isAuthorized (AdminR TestSearchR) _ = return Authorized
    isAuthorized (AdminR StemsR {}) _ = return Authorized
    isAuthorized (AdminR StemCreateFormR {}) _ = return Authorized
    isAuthorized (AdminR StemEditFormR {}) _ = return Authorized
    isAuthorized (AdminR StemR {}) _ = return Authorized
    isAuthorized (AdminR StemsDeleteR {}) _ = return Authorized
    isAuthorized (AdminR OptionsR {}) _ = return Authorized
    isAuthorized (AdminR OptionCreateFormR {}) _ = return Authorized
    isAuthorized (AdminR OptionR {}) _ = return Authorized
    isAuthorized (AdminR OptionEditFormR {}) _ = return Authorized
    isAuthorized (AdminR OptionsDeleteR {}) _ = return Authorized
    isAuthorized (AdminR CandidateCreateFormR) _ = return Authorized
    isAuthorized PhotoPlaceholderR _ = return Authorized
    isAuthorized (AdminR (CandidatePhotoR _)) _ = return Authorized
    isAuthorized (AdminR (CandidateR _)) _ = return Authorized
    isAuthorized (AdminR (CandidateEditFormR _)) _ = return Authorized
    isAuthorized (AdminR CandidatesDeleteR) _ = return Authorized
    isAuthorized (AdminR CandidatesSearchR) _ = return Authorized
    isAuthorized (AdminR (CandidateExamsR _)) _ = return Authorized
    isAuthorized StepR {} _ = return Authorized
    isAuthorized DocsR _ = return Authorized
    isAuthorized DocsErdR _ = return Authorized    
    isAuthorized ExamFormR _ = return Authorized
    isAuthorized ExamR _ = return Authorized
    isAuthorized SignInR _ = return Authorized
    isAuthorized SignOutR _ = return Authorized
    isAuthorized CompleteR {} _ = return Authorized
    isAuthorized SummaryR {} _ = return Authorized
    isAuthorized MyExamsR _ = return Authorized
    isAuthorized SearchExamR _ = return Authorized
    isAuthorized (ExamInfoR _) _ = return Authorized
    isAuthorized (ExamSkillsR _) _ = return Authorized
    isAuthorized TerminateR {} _ = return Authorized
    isAuthorized MyExamR {} _ = return Authorized
    isAuthorized MyExamsSearchR _ = return Authorized
    isAuthorized (AdminR (CandidateSkillsR _)) _ = return Authorized
    isAuthorized (AdminR (TestPublishR _)) _ = return Authorized
    isAuthorized (AdminR (TestUnpublishR _)) _ = return Authorized
    isAuthorized (AdminR CandidatePhotosR) _ = return Authorized
    isAuthorized (ExamTestR {}) _ = return Authorized
    isAuthorized (StatsR TopSkilledR) _ = return Authorized
    isAuthorized (StatsR SkilledR {}) _ = return Authorized
    isAuthorized (StatsR TopExamsR) _ = return Authorized
    isAuthorized (StatsR (TopExamR _)) _ = return Authorized
    isAuthorized (StatsR ExamSuccessRatesR) _ = return Authorized
    isAuthorized (StatsR (TestSuccessRateR _)) _ = return Authorized
    isAuthorized (RemainingTimeR _) _ = return Authorized
    


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App


instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ [] = defaultFormMessage
  renderMessage _ ("en":_) = englishFormMessage
  renderMessage _ ("fr":_) = frenchFormMessage
  renderMessage _ ("ru":_) = russianFormMessage
  renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
