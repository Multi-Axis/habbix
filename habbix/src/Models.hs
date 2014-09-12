{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Models
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Models where

import Data.Text (Text)
import Data.Int
import Database.Persist.TH

import FixedE4

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Host sql=hosts id=hostid
    host Text
    status Int
    available Int
    name Text
    deriving Show

Item sql=items id=itemid
    type Int
    hostid HostId
    name Text
    key_ Text
    UniqItem hostid key_ !sql=items_1
    deriving Show

-- Note: History has no (primary key) id in zabbix db, so we don't provide
-- one either. So please, never @get@ this entity!
History
    itemid Int64
    clock Int
    value FixedE4
    ns Int
    deriving Show
|]
