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
import Database.Persist.TH

import FixedE4

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Group sql=groups id=groupid
    name Text

Host sql=hosts id=hostid
    host Text
    status Int
    available Int
    name Text
    deriving Show

HostGroup sql=hosts_groups id=hostgroupid
    host HostId sql=hostid
    group GroupId sql=groupid
    deriving Show

Application sql=applications id=applicationid
    host HostId sql=hostid
    name Text
    deriving Show

Item sql=items id=itemid
    type Int
    host HostId sql=hostid
    name Text
    key_ Text
    description Text
    valueType Int
    UniqItem host key_ !sql=items_1
    deriving Show

ItemApp sql=items_applications id=itemappid
    app ApplicationId sql=applicationid
    item ItemId sql=itemid
    deriving Show

-- Note: History and HistoryUint have no (primary key) id's in zabbix db,
-- so we don't provide one either. So please, never @get@ these entities!
--
-- Note also that item column in zabbix db doesn't refer the Item table
-- like we do for convenience. It should be disabled after a migrate.
History
    item ItemId sql=itemid
    clock Int
    value FixedE4
    ns Int
    deriving Show

HistoryUint sql=history_uint
    item ItemId sql=itemid
    clock Int
    value Rational -- numeric(20,0)
    ns Int
    deriving Show
|]
