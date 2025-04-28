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

import Data.ByteString (ByteString)
import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord)
import qualified Data.Proxy as DP (Proxy)
import Data.Text (Text, pack, unpack)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, fromText, toText)

import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.PersistValue (PersistValue (PersistText))
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql
    ( sqlType, fromSqlKey, toSqlKey, SqlType (SqlString), PersistFieldSql
    )
import Database.Persist.TH (derivePersistField)

import Text.Hamlet (Html)
import Text.Show (Show, show)
import Text.Read (readMaybe)

import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch
    ( PathMultiPiece, toPathMultiPiece, fromPathMultiPiece
    , PathPiece, toPathPiece, fromPathPiece
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


data TimeUnit = TimeUnitMinute | TimeUnitHour
    deriving (Show, Read, Eq)
derivePersistField "TimeUnit"


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


instance PersistField UUID where
    toPersistValue :: UUID -> PersistValue
    toPersistValue = PersistText . toText

    fromPersistValue :: PersistValue -> Either Text UUID
    fromPersistValue (PersistText x) = case fromText x of
                                         Just uuid -> Right uuid
                                         Nothing -> Left "Invalid UUID"
                                         
    fromPersistValue _ = Left "Invalid UUID"


instance PersistFieldSql UUID where
    sqlType :: DP.Proxy UUID -> SqlType
    sqlType _ = SqlString


instance PathPiece UUID where
    toPathPiece :: UUID -> Text
    toPathPiece = toText

    fromPathPiece :: Text -> Maybe UUID
    fromPathPiece = fromText


newtype Tokens = Tokens [UUID]
    deriving (Show, Read, Eq)

instance PathMultiPiece Tokens where
    toPathMultiPiece :: Tokens -> [Text]
    toPathMultiPiece (Tokens xs) = toText <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Tokens
    fromPathMultiPiece xs = Tokens <$> mapM fromText xs


newtype Candidates = Candidates [CandidateId]
    deriving (Show, Read, Eq)

instance PathMultiPiece Candidates where
    toPathMultiPiece :: Candidates -> [Text]
    toPathMultiPiece (Candidates xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Candidates
    fromPathMultiPiece xs = Candidates <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


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


paramAsc :: Text
paramAsc = "asc"

paramDesc :: Text
paramDesc = "desc"


keyUtlDest :: Text
keyUtlDest = "_ULT"

userSessKey :: Text
userSessKey = "candidate"

msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"
