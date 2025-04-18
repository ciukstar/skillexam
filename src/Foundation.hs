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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Control.Monad ((>>))
import Control.Monad.Logger (LogSource)

import Data.Bool (Bool (False), otherwise)
import qualified Data.CaseInsensitive as CI
import Data.List (length, zip)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Kind (Type)
import Data.Ord ((<))
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, select, not_, unionAll_
    , (^.), (==.)
    )
import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Import.NoFoundation

import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Text.Shakespeare.I18N (mkMessage)

import Yesod.Auth.Message (AuthMessage(InvalidLogin, LoginTitle))
import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Core.Handler
    ( getYesod, defaultCsrfCookieName, defaultCsrfHeaderName
    , withUrlRenderer, languages, HandlerFor, newIdent, getMessages
    , setUltDestCurrent, selectRep, provideRep, getMessageRender
    , getRouteToParent, getUrlRender, lookupSession, setUltDest
    )
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Core
    ( MonadUnliftIO, unauthorizedI, object, (.=)
    , ErrorResponse (NotFound, PermissionDenied, InvalidArgs)
    , TypedContent, Yesod (errorHandler), defaultErrorHandler
    , setTitleI, WidgetFor
    )
import Yesod.Core.Types     (Logger)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Yesod.Form.Types (MForm, FormResult)

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)
import Data.Map (Map)


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
    , exams          :: TVar (Map ExamId (TChan ExamStatus))
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

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a


widgetAccount :: Widget
widgetAccount = do
    user <- maybeAuth
    $(widgetFile "widgets/account")
    

widgetSnackbar :: [(Text,Html)] -> Widget
widgetSnackbar msgs = $(widgetFile "widgets/snackbar")


widgetMainMenu :: Text -> Text -> Widget
widgetMainMenu idOverlay idDialogMainMenu = do
    curr <- getCurrentRoute
    user <- maybeAuth
    idButtonMainMenuClose <- newIdent
    $(widgetFile "widgets/menu")

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
      
      pc <- widgetToPageContent $ do
          -- addStylesheet $ StaticR css_outlined_css
          -- addStylesheet $ StaticR material_components_web_min_css
          -- addScript $ StaticR material_components_web_min_js
          $(widgetFile "default-layout")
      
      langs <- languages
      let lang = fromMaybe "en" . LS.head $ langs 
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    
    isAuthorized PhotoPlaceholderR _ = return Authorized
    
    isAuthorized HomeR _ = setUltDestCurrent >> return Authorized
    isAuthorized DocsR _ = return Authorized
    
    isAuthorized SummaryR {} _ = return Authorized
    isAuthorized (CancelR uid _) _ = isAuthenticatedSelf uid
    isAuthorized (CompleteR uid _ _ _) _ = isAuthenticatedSelf uid
    isAuthorized (StepInvalidR uid _ _) _ = isAuthenticatedSelf uid
    isAuthorized (StepR uid _ _ _) _ = isAuthenticatedSelf uid

    
    isAuthorized r@(SearchExamR uid _) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized r@(SearchExamsR uid) _ = setUltDest r >> isAuthenticatedSelf uid
    
    isAuthorized r@(ExamEnrollmentFormR uid _ _) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized r@(ExamUserEnrollmentR uid _) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized r@(ExamR uid _) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized r@(ExamsR uid) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized ExamsAfterLoginR _ = return Authorized
    isAuthorized ExamsLoginR _ = return Authorized

    isAuthorized SearchTestExamsR _ = return Authorized
    isAuthorized (TestExamR _) _ = return Authorized
    isAuthorized (TestSkillsR _) _ = return Authorized
    isAuthorized TestExamsR _ = return Authorized
    
    
    isAuthorized (SearchTestExamR _) _ = return Authorized
    
    isAuthorized r@(TestExamUserEnrollmentR _ uid) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized (TestExamEnrollmentR _) _ = return Authorized
    isAuthorized r@(TestExamEnrollmentFormR _ uid _) _ = setUltDest r >> isAuthenticatedSelf uid
    isAuthorized (TestExamLoginR _) _ = return Authorized
    isAuthorized (SearchTestExamSkillsR _) _ = return Authorized
    
    isAuthorized r@(StatsR TopSkilledR) _ = setUltDest r >> isAdmin
    isAuthorized (StatsR SkilledR {}) _ = isAdmin
    isAuthorized r@(StatsR TopExamsR) _ = setUltDest r >> isAdmin
    isAuthorized (StatsR (TopExamR _)) _ = isAdmin
    isAuthorized r@(StatsR ExamSuccessRatesR) _ = setUltDest r >> isAdmin
    isAuthorized (StatsR (TestSuccessRateR _)) _ = isAdmin

    
    isAuthorized (WebSocketTimeoutR _) _ = return Authorized
    isAuthorized (RemainingTimeR _) _ = return Authorized

    isAuthorized (DataR SkillCreateFormR) _ = isAdmin
    isAuthorized (DataR SkillsSearchR) _ = isAdmin
    isAuthorized (DataR (SkillsDeleteR _)) _ = isAdmin
    isAuthorized (DataR (SkillR _)) _ = isAdmin
    isAuthorized (DataR (SkillEditFormR _)) _ = isAdmin
    isAuthorized (DataR SkillsR) _ = setUltDestCurrent >> isAdmin
    
    isAuthorized (DataR (TestPublishR _)) _ = isAdmin
    isAuthorized (DataR (TestUnpublishR _)) _ = isAdmin
    isAuthorized (DataR TestCreateFormR) _ = isAdmin
    isAuthorized (DataR TestEditFormR {}) _ = isAdmin
    isAuthorized (DataR TestR {}) _ = isAdmin
    isAuthorized (DataR (TestDeleR _)) _ = isAdmin
    isAuthorized (DataR TestSearchR) _ = isAdmin
    isAuthorized (DataR TestsR) _ = setUltDestCurrent >> isAdmin
    
    isAuthorized (DataR StemsR {}) _ = isAdmin
    isAuthorized (DataR StemCreateFormR {}) _ = isAdmin
    isAuthorized (DataR StemEditFormR {}) _ = isAdmin
    isAuthorized (DataR StemR {}) _ = isAdmin
    isAuthorized (DataR StemsDeleteR {}) _ = isAdmin
    
    isAuthorized (DataR OptionsR {}) _ = isAdmin
    isAuthorized (DataR OptionCreateFormR {}) _ = isAdmin
    isAuthorized (DataR OptionR {}) _ = isAdmin
    isAuthorized (DataR OptionEditFormR {}) _ = isAdmin
    isAuthorized (DataR OptionsDeleteR {}) _ = isAdmin

    isAuthorized (DataR (CandidateSkillsR _)) _ = isAdmin
    isAuthorized (DataR CandidateCreateFormR) _ = isAdmin
    isAuthorized (DataR (CandidateR _)) _ = isAdmin
    isAuthorized (DataR (CandidateEditFormR _)) _ = isAdmin
    isAuthorized (DataR (CandidateDeleR _)) _ = isAdmin
    isAuthorized (DataR CandidatesSearchR) _ = isAdmin
    isAuthorized (DataR (CandidateExamsR _)) _ = isAdmin
    isAuthorized (DataR (CandidateExamR _ _)) _ = isAdmin
    isAuthorized (DataR CandidatesR) _ = setUltDestCurrent >> isAdmin
        
    isAuthorized (DataR (CandidatePhotoR _)) _ = return Authorized
    isAuthorized (DataR CandidatePhotosR) _ = return Authorized

    isAuthorized (DataR (UserResetPasswordR _)) _ = isAdmin
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR UserNewR) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized (DataR UsersR) _ = setUltDestCurrent >> isAdmin
    isAuthorized (DataR (UserPhotoR _)) _ = return Authorized
    
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

    errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    errorHandler NotFound = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPageNotFound
            $(widgetFile "error/not-found")
        provideRep $ return $ object ["message" .= ("Page not found." :: Text)]
        provideRep $ return ("Page not found." :: Text)

    errorHandler (PermissionDenied msg) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPermissionDenied
            $(widgetFile "error/permission-denied")
        provideRep $ do
            msgr <- getMessageRender
            return $ object ["message" .= (msgr MsgPermissionDenied <> "Permission Denied. " <> msg)]
        provideRep $ return $ "Permission Denied. " <> msg

    errorHandler (InvalidArgs msgs) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgInvalidArguments
            idHeader <- newIdent
            $(widgetFile "error/invalid-args")
        provideRep $ return $ object ["message" .= msgs]
        provideRep $ return $ T.intercalate ", " msgs

    errorHandler x = defaultErrorHandler x


isAuthenticatedSelf :: UserId -> Handler AuthResult
isAuthenticatedSelf uid = do
    muid <- maybeAuthId
    case muid of
        Just uid' | uid == uid' -> return Authorized
                  | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
        Nothing -> unauthorizedI MsgLoginPlease

        
isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True _ _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ False _ _ _)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgSignInToAccessPlease
        

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

    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout w = liftHandler $ do
        defaultLayout $ do
            setTitleI MsgSignIn
            $(widgetFile "auth/layout")

    loginHandler :: AuthHandler App Html
    loginHandler = do
        app <- getYesod
        tp <- getRouteToParent
        rndr <- getUrlRender
        backlink <- fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
        let indexes = [1..]
        authLayout $ do
            setTitleI LoginTitle
            msgs <- getMessages
            idButtonBack <- newIdent
            $(widgetFile "auth/login")

    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate (Creds _ ident _) = liftHandler $ do
        user <- runDB $ selectOne $ do
            x <- from $ table @User
            where_ $ x ^. UserEmail ==. val ident
            return x
            
        case user of
          Just (Entity uid _) -> return $ Authenticated uid              
          Nothing -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [ authHashDBWithForm formLogin (Just . UniqueUser)
                    ]


formLogin :: Route App -> Widget
formLogin route = do

    users <- liftHandler $ runDB $ select $ from $
        ( do
              x <- from $ table @User
              where_ $ not_ $ x ^. UserSuper
              return x
        )
        `unionAll_`
        ( do
              x <- from $ table @User
              where_ $ x ^. UserSuper
              return x
        )

    msgs <- getMessages
    idInputUsername <- newIdent
    idInputPassword <- newIdent
    $(widgetFile "auth/form") 
                      

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
