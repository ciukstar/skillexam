{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataFR (populateFR) where

import Control.Monad.IO.Class (MonadIO)
import ClassyPrelude.Yesod (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_) )

import Model (Skill(Skill, skillCode, skillName, skillDescr))

populateFR :: MonadIO m => ReaderT SqlBackend m ()
populateFR = do
    insert_ $ Skill { skillCode = "Jakarta EE"
                    , skillName = "Java Enterprise Edition"
                    , skillDescr = Just "Compétences en programmation en Java Enterprise Edition"
                    }
