{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE InstanceSigs #-}

module Model where

import ClassyPrelude.Yesod
    ( Typeable, Bool, Double, Int, Textarea, Read, Eq, mkMigrate
    , mkPersist, persistFileWith, share, sqlSettings
    , (<$>), (.)
    )
    
import Control.Monad (mapM)

import Data.Maybe (Maybe (Just))
import Data.Ord (Ord)
import Data.Text (Text, pack, unpack)
import Data.Time (Day, UTCTime)
import Data.ByteString (ByteString)

import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.TH (derivePersistField)

import Text.Hamlet (Html)
import Text.Show (Show, show)
import Text.Read (readMaybe)

import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch
    ( PathMultiPiece, toPathMultiPiece, fromPathMultiPiece
    )


data AuthenticationType = UserAuthTypePassword
                        | UserAuthTypeEmail
                        | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"


data TestState = TestStatePublished | TestStateUnpublished
    deriving (Show, Read, Eq)
derivePersistField "TestState"


data StemType = SingleRespose | MultiResponse
    deriving (Show, Read, Eq)
derivePersistField "StemType"


data ExamStatus = ExamStatusOngoing
                | ExamStatusCompleted
                | ExamStatusTimeout
                | ExamStatusCanceled
    deriving (Show, Read, Eq)
derivePersistField "ExamStatus"




-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


newtype Skills = Skills { unSkills :: [SkillId] }
  deriving (Show, Read, Eq)


instance PathMultiPiece Skills where
    toPathMultiPiece :: Skills -> [Text]
    toPathMultiPiece (Skills xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Skills
    fromPathMultiPiece xs = Skills <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }
    

instance SqlString Textarea


keyUtlDest :: Text
keyUtlDest = "_ULT"

userSessKey :: Text
userSessKey = "candidate"


msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"
