------------------------------------------------------------------------------
-- | 
-- Module         : Query
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Query where

import ZabbixDB

import           Data.Ratio (numerator)
import           Data.List.HT (sliceHorizontal)
import           Control.Monad
import           Control.Arrow
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Database.Esqueleto
import qualified Database.Persist as P

valid :: IsSqlKey a => a -> Bool
valid = (>= 0) . fromSqlKey

-- * General queries

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
selectHostApplications hid = 
    P.selectList
        [ApplicationHost P.==. hid | valid hid]
        [ P.Asc ApplicationHost, P.Asc ApplicationName ]

-- | All items for an application. For example, querying for CPU gives
-- items for 1 min avg load, 5 min avg load 15 min avg load, idle time, etc.
--
-- /Discards items that do not show up in the history table/.
selectAppItems :: ApplicationId -> DB [Entity Item]
selectAppItems aid = select . from $ \(itemapp `InnerJoin` item) -> do
    on (itemapp ^. ItemAppItem ==. item ^. ItemId)

    -- pq8 note: in 9.4 a simple (item ^. ItemId) would be enough.
    groupBy ( item ^. ItemId
            , item ^. ItemType
            , item ^. ItemHost
            , item ^. ItemName
            , item ^. ItemKey_
            , item ^. ItemDescription
            , item ^. ItemValueType
            )

    when (valid aid) $ where_ (itemapp ^. ItemAppApp ==. val aid)

    return item

-- * History data points

type DPS val = Source DB (Epoch, val)

-- | Get all history for given item.
selectHistory :: Key Item -> Int -> Either (DPS FixedE4) (DPS Integer)
selectHistory iid vtype =
    unwrap $ case vtype of

        0 -> Left . selectSource . from $ \history -> do
            where_ $ history ^. HistoryItem ==. val iid
            orderBy [asc $ history ^. HistoryClock]
            return (history ^. HistoryClock, history ^. HistoryValue)

        1 -> error "selectHistory: historyString not implemented"
        2 -> error "selectHistory: What the f*ck is value_type = 2?"

        3 -> Right . selectSource . from $ \histUint -> do
            where_ (histUint ^. HistoryUintItem ==. val iid)
            orderBy [asc $ histUint ^. HistoryUintClock]
            return (histUint ^. HistoryUintClock, histUint ^. HistoryUintValue)

        4 -> error "selectHistory: value_type = 4, huh?"
        _ -> error "selectHistory: items.value_type over 4"
    where
        unwrap (Left ints) = Left (ints $= CL.map (unValue *** unValue))
        unwrap (Right fxd) = Right (fxd $= CL.map (unValue *** numerator . unValue))
                                            -- only numerator because
                                            -- numeric(20,0)

-- * Populate


-- | Fetch zabbix data (from remote to local) for all zabbix-tables except
-- history and history_uint.
populateZabbixParts :: Habbix ()
populateZabbixParts = do
    selectRepsert ([] :: [P.Filter Group]) []
    selectRepsert ([] :: [P.Filter Host]) []
    selectRepsert ([] :: [P.Filter HostGroup]) []
    selectRepsert ([] :: [P.Filter Application]) []
    selectRepsert ([] :: [P.Filter Item]) []
    selectRepsert ([] :: [P.Filter ItemApp]) []
    where
        selectRepsert xs ys          = runRemoteDB (P.selectSource xs ys $$ CL.consume)
                                        >>= runLocalDB . mapM_ repsertEntity
        repsertEntity (Entity key v) = P.repsert key v

-- | Fetch zabbix history data (from remote to local) for all items present
-- in the managed_item table.
populateHistory :: Habbix ()
populateHistory = do
    items <- runLocalDB . selectDistinct . from $ \(items `InnerJoin` itemfuture) -> do
        on (items ^. ItemId ==. itemfuture ^. ItemFutureItem)
        return (items ^. ItemId, items ^. ItemValueType)

    forM_ items $ \(Value iid, Value vtype) ->
        case vtype of
            -- history
            0 -> do
                hmax <- fmap unvalueFirst . runLocalDB . select . from $ \history -> do
                    where_ (history ^. HistoryItem ==. val iid)
                    return (max_ (history ^. HistoryClock))

                hs <- runRemoteDB . select . from $ \history -> do
                    where_ (history ^. HistoryClock >. val hmax)
                    return ( history ^. HistoryItem
                           , history ^. HistoryClock
                           , history ^. HistoryValue
                           , history ^. HistoryNs)

                mapM_ (runLocalDB . insertMany_ . map toHistory) $ sliceHorizontal 1000 hs

            -- history_uint
            3 -> do
                intmax <- fmap unvalueFirst . runLocalDB . select . from $ \history -> do
                    where_ (history ^. HistoryUintItem ==. val iid)
                    return (max_ (history ^. HistoryUintClock))

                uints <- runRemoteDB . select . from $ \history -> do
                    where_ (history ^. HistoryUintClock >. val intmax)
                    return ( history ^. HistoryUintItem
                           , history ^. HistoryUintClock
                           , history ^. HistoryUintValue
                           , history ^. HistoryUintNs)

                mapM_ (runLocalDB . insertMany_ . map toHistoryUint) $ sliceHorizontal 1000 uints
            _ -> error "Uknown value_type"
    where
        toHistory (Value i, Value c, Value v, Value n) = History i c v n
        toHistoryUint (Value i,Value c,Value v,Value n) = HistoryUint i c v n

        unvalueFirst [Value (Just x)] = x
        unvalueFirst _                = 0
