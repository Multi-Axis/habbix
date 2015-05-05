{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Query
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Queries built with persistent esqueleto against or db (see models in
-- "Models").
------------------------------------------------------------------------------
module Query
    ( -- * Host and app info
    selectHosts, selectHostApplications, selectAppItems,
    selectItemDashboardData,

    -- * History and trends
    DPS, selectHistory, selectHistory', selectHistoryLast,
    selectHistoryMax, selectZabTrendItem,
    
    -- * Future
    newItemFuture,

    -- * Populate from remote
    populateZabbixParts, populateDefaultFutures, populateAll,

    -- * General utility
    withMaybe, getCurrentEpoch
    ) where

import ZabbixDB

import           Data.List.HT as L (sliceHorizontal)
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Sql (unsafeSqlValue)
import qualified Database.Persist as P
import           Data.Time
import           Data.Text (Text)
import qualified Data.Aeson as A

-- valid :: IsSqlKey a => a -> Bool -- different type class than IsSqlKey in newer versions of persistent
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
    P.selectList [ ApplicationHost P.==. hid | valid hid ]
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

-- | Get, by (hostid, items.name), values for the item to use in the dashboard.
selectItemDashboardData :: HostId -> Text -> DB (M.Map Text A.Value)
selectItemDashboardData h m = do
    res <- getItemFutureId h m
    case res of
        Nothing -> return $ M.fromList ["current_time" A..= (-1 :: Int)]
        Just (itemfut, item, scale) -> do
            (n_e, n_v)                <- fromMaybe (0, 0)                                             <$> selectHistoryLast item
            Entity _ t                <- fromMaybe (Entity undefined $ Threshold itemfut False 0 0 0) <$> P.selectFirst [ThresholdItem P.==. itemfut] []
            (past7d, next24h, next6d) <- fromMaybe (-1, -1, -1)                                       <$> selectAggregatedValues item itemfut
            return $ M.fromList $
                [ "metric_name"        A..= m
                , "metric_scale"       A..= scale

                , "current_time"       A..= n_e
                , "current_value"      A..= (fromRational n_v :: Double)

                , "past7d"             A..= past7d
                , "next24h"            A..= next24h
                , "next6d"             A..= next6d

                , "threshold_high"     A..= thresholdHigh t
                , "threshold_warning"  A..= thresholdWarning t
                , "threshold_critical" A..= thresholdCritical t
                , "threshold_lower"    A..= thresholdLower t
                ]

-- (ItemFutureId, ItemId, MetricScale) by (HostId, MetricName)
getItemFutureId :: HostId -> Text -> DB (Maybe (ItemFutureId, ItemId, Double)) -- (, , metric scale)
getItemFutureId hostid metricName = do
    res <- select . from $ \(host `InnerJoin` item `InnerJoin` metric `InnerJoin` itemFuture) -> do
        on (item^.ItemId ==. itemFuture^.ItemFutureItem)
        on (metric^.MetricKey_ ==. item^.ItemKey_)
        on (host^.HostId ==. item^.ItemHost)
        where_ (host^.HostId ==. val hostid &&. metric^.MetricName ==. val metricName)
        orderBy [desc $ itemFuture^.ItemFutureIsMaster]
        return (itemFuture^.ItemFutureId, item^.ItemId, metric^.MetricScale)
    return $ case res of
                 (Value itf, Value i, Value m) : _ -> Just (itf, i, m)
                 _ -> Nothing

-- (past 7d, next24h, next7d)
selectAggregatedValues :: ItemId -> ItemFutureId -> DB (Maybe (Double, Double, Double))
selectAggregatedValues item itemfuture = do
    res <- select $ from $ \(h, f1, f2) -> do
        where_ $ h ^.TrendItem  ==. val item       &&.
                 f1^.FutureItem ==. val itemfuture &&.
                 f2^.FutureItem ==. val itemfuture &&.
                 h ^.TrendClock  >. unsafeSqlValue "EXTRACT(EPOCH FROM current_timestamp) - 7*86400" &&.
                 f1^.FutureClock >. unsafeSqlValue "EXTRACT(EPOCH FROM current_timestamp)" &&.
                 f1^.FutureClock <. unsafeSqlValue "EXTRACT(EPOCH FROM current_timestamp) + 86400" &&.
                 f2^.FutureClock >. unsafeSqlValue "EXTRACT(EPOCH FROM current_timestamp)" &&.
                 f2^.FutureClock <. unsafeSqlValue "EXTRACT(EPOCH FROM current_timestamp) + 7*86400"
        return (max_ (h^.TrendValueMax), max_ (f1^.FutureValue), max_ (f2^.FutureValue))
    return $ case res of
        [(Value (Just a), Value (Just b), Value (Just c))] -> Just (fromRational a, fromRational b, fromRational c)
        _ -> Nothing

-- * History data points

type DPS = Source DB (Epoch, Rational)

-- | Get all history for given item.
selectHistory :: ItemId -> DPS
selectHistory iid = selectHistory' iid undefined Nothing Nothing

-- | Get all history for given item, vtype == 0 or 3.
selectHistory' :: ItemId
               -> Epoch -- ^ Current time (for relative From)
               -> Maybe Epoch -- ^ From
               -> Maybe Epoch -- ^ To
               -> DPS
selectHistory' iid time a b =
    let hists = selectSource . from $ \history -> do
            where_ $ history ^. HistoryItem ==. val iid
            queryParams time a b HistoryClock history
            orderBy [asc $ history ^. HistoryClock]
            return (history ^. HistoryClock, history ^. HistoryValue)

        trends = selectSource . from $ \trend -> do
            where_ $ trend ^. TrendItem ==. val iid
            queryParams time a b TrendClock trend
            orderBy [asc $ trend ^. TrendClock]
            return (trend ^. TrendClock, trend ^. TrendValueAvg) -- TODO min/max/avg

    in mapOutput (\(Value x, Value y) -> (x, y)) (trends <> hists)

-- | This looks at pStopLower and pStopUpper on the params and builds
-- queries to filter history (first part of the tuple) or
-- historyUint (snd) columns based on them.
queryParams :: (PersistEntity val, Esqueleto m expr backend)
            => Epoch -- ^ Today (where pStopLower is relative to when it is < 0)
            -> Maybe Epoch
            -> Maybe Epoch
            -> EntityField val Epoch
            -> expr (Entity val)
            -> m ()
queryParams today pStopLower pStopUpper field h = do
    withMaybe pStopLower $ \l -> where_ $ h ^. field >=. val (if l < 0 then today + l else l)
    withMaybe pStopUpper $ \u -> where_ $ h ^. field <=. val u

selectHistoryMax :: ItemId -> DB (Maybe Rational)
selectHistoryMax iid = fmap unwrap . select . from $ \history -> do
    where_ (history ^. HistoryItem ==. val iid)
    return $ max_ (history ^. HistoryValue)
    where
        unwrap [(Value n)] = n
        unwrap _           = Nothing

selectHistoryLast :: ItemId -> DB (Maybe (Epoch, Rational))
selectHistoryLast iid = fmap unwrapFirst . select . from $ \history -> do
    where_ (history ^. HistoryItem ==. val iid)
    orderBy [desc (history ^. HistoryClock)] 
    limit 1
    return (history ^. HistoryClock, history ^. HistoryValue)

selectTrendLast :: ItemId -> DB (Maybe (Epoch, Rational))
selectTrendLast iid = fmap unwrapFirst . select . from $ \trend -> do
    where_ (trend ^. TrendItem ==. val iid)
    orderBy [desc (trend ^. TrendClock)] 
    limit 1
    return (trend ^. TrendClock, trend ^. TrendValueAvg)

selectItemsWithFuture :: DB [(Value ItemId, Value Int)]
selectItemsWithFuture = selectDistinct . from $ \(items `InnerJoin` itemfuture) -> do
        on (items ^. ItemId ==. itemfuture ^. ItemFutureItem)
        return (items ^. ItemId, items ^. ItemValueType)

-- * Populate

newItemFuture :: ItemId -> FutureModelId -> Bool -> Habbix ItemFutureId
newItemFuture i m = runLocalDB . P.insert . ItemFuture i m "{}" "{}"

-- | Fetch zabbix data (from remote to local) for all zabbix-tables except
-- history and trends
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

-- | Adds default item_futures to those items that do not have any.
--
-- Items to add must satisfy:
--    - key_ must be in metric table
--    - hostid must not be in the Templates host group (groupid = 1)
--    - the itemid must not be present in any row of item_future
populateDefaultFutures :: Habbix ()
populateDefaultFutures = do
    -- find items that do not have futures
    nf <- runLocalDB $ select $ from $ \item -> do
        where_ $ item ^. ItemKey_ `in_`
                subList_selectDistinct (from $ \metric -> return (metric ^. MetricKey_))
            &&. item ^. ItemHost `in_`
                subList_selectDistinct (from $ \hg -> do where_ (hg ^. HostGroupGroup !=. val (toSqlKey 1))
                                                         return (hg ^. HostGroupHost)
                                       )
            &&. item ^. ItemId `notIn`
                subList_selectDistinct (from $ \f -> return $ f ^. ItemFutureItem)
        return (item ^. ItemId)

    -- check if there is a model to assign
    mmod <- runLocalDB $ P.selectFirst [] []
    case mmod of
        Nothing -> logErrorN "There are no models in db. Cannot assign futures. Please add a model first"
        Just (Entity m _) -> mapM_ (\(Value i) -> newItemFuture i m True) nf

-- | Fetch zabbix history data (from remote to local) for all items present
-- in the item_future table.
populateAll :: Habbix ()
populateAll = do
    items <- runLocalDB selectItemsWithFuture
    forM_ items $ \(Value i, Value t) -> do
        logDebugN $ "Populating itemid = " <> tshow i
        populateHistoryFor i t
        populateTrendsFor i t

populateHistoryFor :: ItemId -> Int -> Habbix ()
populateHistoryFor iid vtype = do
    -- Get last tick
    lastClock <- runLocalDB (selectHistoryLast iid) >>= \x -> case x of
        Just (n, _) -> return n
        Nothing -> do
            logInfoN ("Item (itemid = " <> tshow iid <> ") has no history yet. Populating it from scratch. This may take a while")
            return 0
    twoWeeks <- liftIO (daysAgo 14)

    let toHistory (Value c, Value v) = History iid c v

    -- delete older than two weeks
    runLocalDB . delete . from $ \h -> where_ (h ^. HistoryClock <. val twoWeeks)

    vals <- runRemoteDB $ case vtype of
        0 -> selectZabHist iid (max lastClock twoWeeks)
        3 -> selectZabHistUint iid (max lastClock twoWeeks)
        _ -> do
            logErrorN $ "Item (itemid = " <> tshow iid <> ") has unknown value_type (" <> tshow vtype <> ")"
            return []

    insertMany_' $ map toHistory vals

populateTrendsFor :: ItemId -> Int -> Habbix ()
populateTrendsFor iid vtype = do
    lastClock <- runLocalDB (selectTrendLast iid) >>= \x -> case x of
        Just (n, _) -> return n
        Nothing -> do
            logInfoN ("Item (itemid = " <> tshow iid <> ") has no trend history yet. Populating it from scratch. This may take a while")
            return 0
    vals <- runRemoteDB $ case vtype of
        0 -> selectZabTrend iid lastClock
        3 -> selectZabTrendUint iid lastClock
        _ -> do
            logErrorN $ "Item (itemid = " <> tshow iid <> ") has unknown value_type (" <> tshow vtype <> ")"
            return []
    let toTrend (Value clock, Value a, Value b, Value c) = Trend iid clock a b c
    insertMany_' $ map toTrend vals

insertMany_' :: (PersistEntity p, PersistEntityBackend p ~ Connection) => [p] -> Habbix ()
insertMany_' = mapM_ (runLocalDB . insertMany_) . L.sliceHorizontal 1000

-- * Zab

selectZabHist, selectZabHistUint :: ItemId -> Epoch -> DB [(Value Int, Value Rational)]
selectZabHist iid lc = select . from $ \history -> do
    where_ $ history ^. ZabHistClock >. val lc
         &&. history ^. ZabHistItem ==. val iid
    return (history ^. ZabHistClock, history ^. ZabHistValue)
selectZabHistUint iid lc = select . from $ \history -> do
    where_ $ history ^. ZabHistUintClock >. val lc
         &&. history ^. ZabHistUintItem ==. val iid
    return (history ^. ZabHistUintClock, history ^. ZabHistUintValue)

-- | Select trends data from zabbix
selectZabTrendsFor :: ItemId -> Int -> Epoch -> DB [Trend]
selectZabTrendsFor iid vtype lastClock = liftM (map toTrend) $ case vtype of
    0 -> selectZabTrend iid lastClock
    3 -> selectZabTrendUint iid lastClock
    _ -> error $ "Unknown items.value_type (" ++ show vtype ++ ") in selectZabTrendsFor (itemid = " ++ show (fromSqlKey iid) ++ ")"
  where
    toTrend (Value time, Value mi, Value av, Value ma) = Trend iid time mi av ma

selectZabTrend, selectZabTrendUint :: ItemId -> Epoch -> DB [(Value Int, Value Rational, Value Rational, Value Rational)]
selectZabTrend iid lc = select . from $ \trend -> do
    where_ $ trend ^. ZabTrendClock >. val lc
         &&. trend ^. ZabTrendItem ==. val iid
    return (trend ^. ZabTrendClock, trend ^. ZabTrendValueMin, trend ^. ZabTrendValueAvg, trend ^. ZabTrendValueMax)
selectZabTrendUint iid lc = select . from $ \trend -> do
    where_ $ trend ^. ZabTrendUintClock >. val lc
         &&. trend ^. ZabTrendUintItem ==. val iid
    return (trend ^. ZabTrendUintClock, trend ^. ZabTrendUintValueMin, trend ^. ZabTrendUintValueAvg, trend ^. ZabTrendUintValueMax)

selectZabTrendItem :: ItemId -> DB (Entity Host, Entity Item, [Trend])
selectZabTrendItem iid = do
    item <- P.getJust iid
    host <- P.getJust (itemHost item)
    trends <- selectZabTrendsFor iid (itemValueType item) 0
    return (Entity (itemHost item) host, Entity iid item, trends)

-- * Utility

daysAgo :: Int -> IO Epoch
daysAgo n = (\x -> x - n * 86400) <$> getCurrentEpoch

getCurrentEpoch :: IO Epoch
getCurrentEpoch = read . formatTime undefined "%s" <$> getCurrentTime

unwrapFirst :: [(Value a, Value b)] -> Maybe (a, b)
unwrapFirst ((Value e, Value n) : _) = Just (e, n)
unwrapFirst _ = Nothing

withMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
withMaybe m f = maybe (return ()) f m
