------------------------------------------------------------------------------
-- | 
-- Module         : Query
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Query where

import ZabbixDB

import Control.Monad
import Control.Arrow
import Database.Esqueleto
import qualified Database.Persist as P

-- | Select all hosts along with their groups that do /not/ belong to the
-- Templates (groupid == 1).
selectHosts :: DB [(Entity Group, Entity Host)]
selectHosts = select . from $ \(group `InnerJoin` hostGroup `InnerJoin` host) -> do
    on (hostGroup ^. HostGroupHost ==. host ^. HostId)
    on (group ^. GroupId ==. hostGroup ^. HostGroupGroup)
    where_ (group ^. GroupId !=. val (toSqlKey 1))
    return (group, host)

-- | All apps for the host (cpu, memory, network, fs, ...).
selectHostApplications :: HostId -> DB [Entity Application]
selectHostApplications hid = P.selectList [ApplicationHost P.==. hid] [P.Asc ApplicationName]

-- | All items for an application. For example, querying for CPU gives
-- items for 1 min avg load, 5 min avg load 15 min avg load, idle time, etc.
--
-- /Discards items that do not show up in the history table/.
selectAppItems :: ApplicationId -> DB [Entity Item]
selectAppItems aid = select . from $ \(itemapp `InnerJoin` item `InnerJoin` hist) -> do
    on (hist ^. HistoryItem ==. item ^. ItemId)
    on (itemapp ^. ItemAppItem ==. item ^. ItemId)
    groupBy (item ^. ItemId)
    where_ (itemapp ^. ItemAppApp ==. val aid)
    return item

-- | Get all history for given item id.
selectHistory :: ItemId -> DB [(Epoch, FixedE4)]
selectHistory iid = liftM (map $ unValue *** unValue) . select . from $ \history -> do
    where_ $ history ^. HistoryItem ==. val iid
    orderBy [asc $ history ^. HistoryClock]
    return (history ^. HistoryClock, history ^. HistoryValue)

