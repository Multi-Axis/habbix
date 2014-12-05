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
--
-- Models in our db, and possibly in the zabbix db. Also provides
-- 'migrateAll' that migrates an empty or partial db schema to ours.
--
-- __Note that we do not provide any indices, that are crucial for
-- multi-axis-graphs dashboard performance.__ Refer to that documentation
-- no how to set them up.
------------------------------------------------------------------------------
module Models where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.TH

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

-- Notes:
--
-- - We combine the `history` (values are numeric(20,4)) and `history_uint`
--   (values are unsigned ints) tables from zabbix into this History table
--   (values are always Rational).
--
-- - `history` (and history_uint`) have no (primary key) id's in zabbix
--   db, and there's no reason for us to provide them either. So please,
--   never @SELECT@ the id column (though the migrate creates it).
--
-- - The itemid column in zabbix db doesn't refer the Item table like we
--   do for convenience.
History
    item        ItemId    sql=itemid
    clock       Int
    value       Rational  sqltype=numeric(20,4)
    deriving Show

-- TODO: should this contain an 'interval' column?
Trend
    item      ItemId sql=itemid
    clock     Int
    valueMin  Rational  sqltype=numeric(20,4)
    valueAvg  Rational  sqltype=numeric(20,4)
    valueMax  Rational  sqltype=numeric(20,4)


-- Below: NOT in Zabbix

-- Discovering binaries in ./future_models/<name>
FutureModel
    name        Text
    UniqueFutureModel name
    deriving Show

-- History data is updated only for items in this table.
ItemFuture
    item        ItemId          sql=itemid
    model       FutureModelId   sql=modelid
    params      ByteString
    details     ByteString
    isMaster    Bool            default=false -- Is this the default ItemFuture for the item
    deriving Show

-- wrt history
Future
    item        ItemFutureId    sql=itemid
    clock       Int
    value       Rational        sqltype=numeric(20,4)
    deriving Show

-- like 'y = threshold' lines in graphs
Threshold
    item        ItemFutureId    sql=itemid
    normal      Rational        sqltype=numeric(20,4)
    warning     Double    -- trigger if warning  * normal > value
    critical    Double    -- trigger if critical * normal > value
    deriving Show

-- attach pretty names (cpu, mem) to zabbix items
Metric
    name        Text
    key_        Text
    scale       Int
    UniqueMetricName name
    deriving Show
|]
