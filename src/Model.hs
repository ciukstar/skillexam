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

import Control.Monad (mapM)
import Data.Maybe (Maybe)
import Data.Text (pack, unpack)
import Text.Show (show)
import Text.Read (readMaybe)
import Data.Time (Day, UTCTime)
import Data.ByteString (ByteString)
import ClassyPrelude.Yesod
    ( Typeable, Bool, Double, Int, Textarea, Text, mkMigrate
    , mkPersist, persistFileWith, share, sqlSettings, Show, Read, Eq
    , (<$>), (.)
    )
import Yesod.Core.Dispatch (PathMultiPiece, toPathMultiPiece, fromPathMultiPiece)
import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.TH (derivePersistField)

data TestState = TestStatePublished | TestStateUnpublished
    deriving (Show, Read, Eq)
derivePersistField "TestState"


data StemType = SingleRespose | MultiResponse
    deriving (Show, Read, Eq)
derivePersistField "StemType"


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
    


instance SqlString Textarea

ultDestKey :: Text
ultDestKey = "_ULT"

userSessKey :: Text
userSessKey = "candidate"
