{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Models
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Models where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.TH

import FixedE4

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Group sql=groups
    Id                  sql=groupid
    name Text

Host sql=hosts
    Id                  sql=hostid
    host        Text
    status      Int
    available   Int
    name        Text
    deriving Show

HostGroup sql=hosts_groups
    Id              sql=hostgroupid
    host    HostId  sql=hostid
    group   GroupId sql=groupid
    deriving Show

Application sql=applications
    Id             sql=applicationid
    host    HostId sql=hostid
    name    Text
    deriving Show

Item sql=items
    Id                  sql=itemid
    type        Int
    host        HostId  sql=hostid
    name        Text
    key_        Text
    description Text
    valueType   Int
    UniqItem host key_ !sql=items_1
    deriving Show

ItemApp sql=items_applications
    Id                      sql=itemappid
    app     ApplicationId   sql=applicationid
    item    ItemId          sql=itemid
    deriving Show

-- Note: History and HistoryUint have no (primary key) id's in zabbix db,
-- so we don't provide one either. So please, never @get@ these entities!
--
-- Note also that item column in zabbix db doesn't refer the Item table
-- like we do for convenience. It should be disabled after a migrate.
History
    item    ItemId  sql=itemid
    clock   Int
    value   FixedE4
    ns      Int
    deriving Show

HistoryUint sql=history_uint
    item    ItemId  sql=itemid
    clock   Int
    value   Rational -- numeric(20,0)
    ns      Int
    deriving Show


-- NOT in Zabbix

FutureModel
    name        Text
    UniqueFutureModel name
    deriving Show

-- History data is updated only for items in this table.
ItemFuture
    item        ItemId          sql=itemid
    model       FutureModelId   sql=modelid
    params      ByteString
    deriving Show

-- wrt history
Future
    item        ItemFutureId    sql=itemid
    clock       Int
    value       FixedE4
    deriving Show

-- wrt history_uint
FutureUint
    item        ItemFutureId    sql=itemid
    clock       Int
    value       Rational
    deriving Show
|]
