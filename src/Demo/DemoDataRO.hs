{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataRO (populateRO) where

import Control.Monad.IO.Class (MonadIO)
import ClassyPrelude.Yesod (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_) )

import Model (Skill(Skill, skillCode, skillName, skillDescr))

populateRO :: MonadIO m => ReaderT SqlBackend m ()
populateRO = do
    insert_ $ Skill { skillCode = "Jakarta EE"
                    , skillName = "Java Enterprise Edition"
                    , skillDescr = Just "Cunoștințe de programare în Java Enterprise Edition"
                    }
