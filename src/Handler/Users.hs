{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Users
  ( getUsersR, postUsersR
  , getUserR, postUserR
  , getUserPhotoR
  , postUserDeleR
  , getUserEditR
  , getUserNewR
  , getUserResetPasswordR
  , postUserResetPasswordR
  ) where


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (==.), (=.)
    , Value (unValue), orderBy, asc, just
    )    
import Database.Persist
    ( Entity (Entity), entityVal, insert, upsert)
import qualified Database.Persist as P ((=.), delete)

import Foundation
    ( Handler, Form, widgetMainMenu, widgetAccount, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR
      , UserResetPasswordR
      )
    , AppMessage
      ( MsgUsers, MsgPhoto, MsgUser, MsgAdministrator, MsgEmail, MsgName
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel, MsgYes
      , MsgNo, MsgAttribution, MsgPassword, MsgSave, MsgAlreadyExists
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted
      , MsgChangePassword, MsgRecordEdited, MsgEdit, MsgClose
      , MsgSuperuser, MsgPasswordChange, MsgSuperuserCannotBeDeleted
      , MsgRepeatPassword, MsgPasswordDoesNotMatch, MsgPasswordChanged
      , MsgUploadPhoto, MsgTakePhoto
      )
    )

import Material3 (md3widget, md3switchWidget)
    
import Model
    ( msgSuccess, msgError
    , UserId
    , User
      ( User, userName, userEmail, userAdmin, userAuthType, userVerkey
      , userVerified
      )
    , UserPhoto (UserPhoto)
    , AuthenticationType (UserAuthTypePassword)
    , EntityField
      ( UserPhotoUser, UserId, UserPhotoAttribution, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserName, UserAdmin, UserAuthType, UserVerkey
      , UserVerified, UserPassword
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import Yesod.Auth.Email (saltPass)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, whamlet
    , TypedContent (TypedContent), ToContent (toContent), redirect
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), addMessageI, fileSourceByteString
    )
import Yesod.Form.Fields
    ( emailField, textField, fileField, passwordField, htmlField
    , checkBoxField
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId, fvLabel, fvInput, fvErrors, fvRequired )
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postUserResetPasswordR :: UserId -> Handler Html
postUserResetPasswordR uid = do
    ((fr,fw),et) <- runFormPost formPassword
    case fr of
      FormSuccess (pwd,_) -> do
          salted <- liftIO $ saltPass pwd
          runDB $ update $ \x -> do
              set x [UserPassword =. just (val salted)]
              where_ $ x ^. UserId ==. val uid

          addMessageI msgSuccess MsgPasswordChanged
          redirect $ DataR $ UserR uid
          
      _otherwise -> do
          addMessageI msgSuccess MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgUser
              $(widgetFile "data/users/pwd")


getUserResetPasswordR :: UserId -> Handler Html
getUserResetPasswordR uid = do
    (fw,et) <- generateFormPost formPassword
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/pwd")
    

formPassword :: Form (Text,Text)
formPassword extra = do
    
    (pwdR,pwdV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing
        
    (repeatR,repeatV) <- let samePasswordField = flip checkM passwordField $ \a -> do
                                 case pwdR of
                                   FormSuccess pwd | a == pwd -> return $ Right a
                                   FormSuccess pwd | a /= pwd -> return $ Left MsgPasswordDoesNotMatch
                                   _otherwise -> return $ Right a
                         in mreq samePasswordField FieldSettings
                            { fsLabel = SomeMessage MsgRepeatPassword
                            , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
                            } Nothing
        
    let w = [whamlet|
                    ^{extra}
                    ^{md3widget pwdV}
                    ^{md3widget repeatV}
                    |]
    return ((,) <$> pwdR <*> repeatR, w)


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = do
    ((fr,_),_) <- runFormPost formUserDelete
    case fr of
      FormSuccess () -> do
          user <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserId ==. val uid
              return x

          case user of
            Just (Entity _ (User _ _ _ True _ _ _ _)) -> do
                addMessageI msgError MsgSuperuserCannotBeDeleted
                redirect $ DataR $ UserR uid
            _otherwise -> return ()
              
          runDB $ P.delete uid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR UsersR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserR uid


getUserEditR :: UserId -> Handler Html
getUserEditR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    (fw,et) <- generateFormPost $ formUser user

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/edit")


getUserNewR :: Handler Html
getUserNewR = do

    (fw,et) <- generateFormPost $ formUser Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/new")


postUserR :: UserId -> Handler Html
postUserR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    
    ((fr,fw),et) <- runFormPost $ formUser user

    case fr of
      FormSuccess (User email _ name _ admin authType vekey verified,(Just fi,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  , UserAuthType =. val authType
                  , UserVerkey =. val vekey
                  , UserVerified =. val verified
                  ]
            where_ $ x ^. UserId ==. val uid
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs attrib)
                    [ UserPhotoMime P.=. fileContentType fi
                    , UserPhotoPhoto P.=. bs
                    , UserPhotoAttribution P.=. attrib
                    ]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UserR uid
          
      FormSuccess (User email _ name _ admin authType vekey verified,(Nothing,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  , UserAuthType =. val authType
                  , UserVerkey =. val vekey
                  , UserVerified =. val verified
                  ]
            where_ $ x ^. UserId ==. val uid
          void $ runDB $ update $ \x -> do
              set x [UserPhotoAttribution =. val attrib]
              where_ $ x ^. UserPhotoUser ==. val uid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UserR uid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              $(widgetFile "data/users/edit")


getUserR :: UserId -> Handler Html
getUserR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    (fw0,et0) <- generateFormPost formUserDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/users/user")


formUserDelete :: Form ()
formUserDelete extra = return (pure (), [whamlet|#{extra}|])


postUsersR :: Handler Html
postUsersR = do

    ((fr,fw),et) <- runFormPost $ formUser Nothing

    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          uid <- runDB $ insert r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs attrib)
                    [ UserPhotoMime P.=. fileContentType fi
                    , UserPhotoPhoto P.=. bs
                    , UserPhotoAttribution P.=. attrib
                    ]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR
          
      FormSuccess (r,(Nothing,attrib)) -> do
          uid <- runDB $ insert r
          void $ runDB $ update $ \x -> do
              set x [UserPhotoAttribution =. val attrib]
              where_ $ x ^. UserPhotoUser ==. val uid
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              $(widgetFile "data/users/new")


getUsersR :: Handler Html
getUsersR = do

    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), asc (x ^. UserEmail), asc (x ^. UserId)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUsers
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/users/users")


formUser :: Maybe (Entity User) -> Form (User,(Maybe FileInfo,Maybe Html))
formUser user extra = do

    (emailR,emailV) <- mreq uniqueEmailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userEmail . entityVal <$> user)

    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userName . entityVal <$> user)

    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userAdmin . entityVal <$> user)

    let authTypeR = pure $ maybe UserAuthTypePassword (userAuthType . entityVal) user
    let verkeyR = pure $ userVerkey . entityVal =<< user
    let verifiedR = pure $ maybe False (userVerified . entityVal) user

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    attrib <- (unValue =<<) <$> case user of
      Just (Entity uid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @UserPhoto
          where_ $ x ^. UserPhotoUser ==. val uid
          return $ x ^. UserPhotoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = (,) <$> ( User <$> emailR <*> pure Nothing <*> nameR <*> pure False <*> adminR
                      <*> authTypeR <*> verkeyR <*> verifiedR
                    )
                <*> ((,) <$> photoR <*> attribR)

    idOverlay <- newIdent
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idDialogSnapshot <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCapture <- newIdent

    return (r, $(widgetFile "data/users/form"))
  where
      uniqueEmailField :: Field Handler Text
      uniqueEmailField = checkM uniqueEmail emailField

      uniqueEmail :: Text -> Handler (Either AppMessage Text)
      uniqueEmail email = do
          x <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail ==. val email
              return x
          return $ case x of
            Nothing -> Right email
            Just (Entity rid _) -> case user of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right email
                                   | otherwise -> Left MsgAlreadyExists


getUserPhotoR :: UserId -> Handler TypedContent
getUserPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
