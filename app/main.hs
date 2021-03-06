{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Main (main.hs)
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main (main) where

import           ZabbixDB hiding (Future, History)
import           Query
import           Future

import           Prelude
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson hiding (Result)
import qualified Data.Map.Strict            as M
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe
import           Data.Text          (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time
import qualified Data.Vector                as DV
import qualified Data.Vector.Storable       as V
import qualified Data.Yaml                  as Yaml
import           Data.Int
import qualified Database.Esqueleto         as E
import           Database.Esqueleto              (toSqlKey, Entity(..))
import qualified Database.Persist           as P
import           System.Console.CmdArgs
import           System.Directory (doesFileExist)
import           Text.Printf

-- | Inserted by MigrateDb
defaultMetricNames :: [Metric]
defaultMetricNames =
    [ Metric "mem" "vm.memory.size[available]"    (1024 ^ 3) -- a gB
    , Metric "cpu" "system.cpu.load[percpu,avg5]" 1          -- %
    , Metric "swap" "system.swap.size[,pfree]"    (1024 ^ 3)
    , Metric "fsroot" "vfs.fs.size[/,pfree]"      (1024 ^ 3)
    ]

type DashboardConfig = M.Map Text [Text]

data Config = Config
            { localDatabase :: ConnectionString
            , zabbixDatabase :: ConnectionString
            , modeldir :: FilePath
            , dashboardConfig :: DashboardConfig
            }

instance FromJSON Config where
    parseJSON (Object o) = Config <$> (encodeUtf8 <$> o .: "localDatabase")
                                  <*> (encodeUtf8 <$> o .: "zabbixDatabase")
                                  <*> (fromMaybe "forecast_models" <$> o .:? "modeldir")
                                  <*> (o .: "dashboard")
    parseJSON _          = mzero

data Program = Hosts     { config :: String, outType :: DataOutType }
             | Apps      { config :: String, outType :: DataOutType, argid :: Int64 }
             | Items     { config :: String, outType :: DataOutType, argid :: Int64 }
             | History   { config :: String, outType :: DataOutType, argid :: Int64, samples :: Int }
             | Trends    { config :: String, outType :: DataOutType, argid :: Int64, asItem :: Int64, asHost :: Int64 }
             | Future    { config :: String, outType :: DataOutType }
             | Models    { config :: String, outType :: DataOutType }
             | MigrateDb { config :: String, outType :: DataOutType }
             | Sync      { config :: String, outType :: DataOutType, syncAll :: Bool, itemsToSync :: [Int64] }
             | Configure { config :: String, outType :: DataOutType, item :: Int64, model :: Int64, executable :: String }
             | Execute   { config :: String, outType :: DataOutType, argid :: Int64, params :: String, outCombine :: Bool }
             | Compare   { config :: String, outType :: DataOutType, argid :: Int64, fromInterval :: (Epoch, Epoch), toInterval :: (Epoch, Epoch) }
             | Dashboard { config :: String, outType :: DataOutType, cached :: Bool }
             | Th        { config :: String, outType :: DataOutType, update :: Bool, metric :: Maybe String, critical :: Maybe Double, warning :: Maybe Double, high :: Maybe Double, lower :: Maybe Bool }
             deriving (Show, Data, Typeable)

data DataOutType = OutHuman | OutJSON | OutSQL
                 deriving (Eq, Show, Data, Typeable)

prgConf :: Program
prgConf = modes
    -- Query only
    [ Hosts     { outType = enum [ OutHuman &= name "human" &= help "Human-readable JSON output"
                                 , OutJSON  &= name "json"  &= help "Bare JSON output"
                                 , OutSQL   &= name "sql"   &= help "SQL dump output"
                                 ]
                , config  = "config.yaml" &= typFile &= help "yaml config file (default: ./config.yaml)"
                } &= help "List all hosts and groups except templates"
    , Apps      { argid   = (-1) &= argPos 0 &= typ "ID"
                } &= help "List available \"metric groups\" for the Host ID"
    , Items     { } &= help "List available \"metrics\" in the metric group App ID>"
    , History   { samples = 80 &= help "Sample resolution (default 80)"
                } &= help "Print history data for <itemid>"
    , Trends    { asItem  = (-1) &= help "Output the trend data with this item id"
                , asHost  = (-1) &= help "Output trend data with this host id"
                } &= help "Print trend data for the REMOTE item"
    , Future    {
                } &= help "List all item futures"
    , Models    {
                } &= help "List available future models"
    -- Database modifying actions
    , MigrateDb {
                } &= help "Create or update the local DB schema"
    , Sync      { syncAll     = False &= help "Sync every table, not just history"
                , itemsToSync = [] &= help "Optional item_future.id's to sync"
                } &= help "Synchronize remote db with local and run futures"
    , Configure { executable = ""   &= typFile        &= help "Register a new forecast model"
                , item       = (-1) &= typ "ITEMID"   &= help "Create a new item_future"
                , model      = (-1) &= typ "MODELID"  &= help "Model to register on the new item_future"
                } &= help "Configure the predictions in database"
    -- Info
    , Execute   { -- argid
                  params = "" &= typ "JSON"
                , outCombine = False &= help "Combine clock/value in the output"
                } &= help "Execute item_future.ID but only output the results, instead of modifying database"
    , Compare   { -- argid
                  fromInterval = def &= help "Interval to use with predictions"
                , toInterval   = def &= help "Interval to compare the predicted model to"
                } &= help "Compare predictions from knowing A to an actual history B"
    , Dashboard { cached = True &= help "Use cached version"
                } &= help "Print dashboard-y information"

    , Th        { update   = False &= help "Make changes in the database"
                , metric   = Nothing &= help "Which metric to operate on"
                , lower    = Nothing &= help "Are the threshold lower bounds (default: upper)"
                , critical = Nothing &= help "critical threshold"
                , warning  = Nothing &= help "warning threshold"
                , high     = Nothing &= help "high threshold"
                } &= help "Print or set threshold values"
    ] &= program "habbix" &= verbosity

main :: IO ()
main = do
    prg <- cmdArgs prgConf
    Just Config{..} <- Yaml.decodeFile (config prg)
    debugInfo <- isLoud
    bequiet <- not <$> isNormal

    let argid'  | argid prg < 0  = error "ItemID must be >= 0"
                | otherwise      = toSqlKey $ argid prg
        asItem' | asItem prg < 0 = error "asItem must be >= 0"
                | otherwise      = toSqlKey $ asItem prg :: ItemId
        asHost' | asHost prg < 0 = error "asHost must be >= 0"
                | otherwise      = toSqlKey $ asHost prg :: HostId

        out x = liftIO . BLC.putStrLn $ case outType prg of
            OutJSON  -> encode $ outJSON x
            OutHuman -> outHuman x
            OutSQL   -> outSQL x

    runHabbix bequiet debugInfo modeldir localDatabase zabbixDatabase $ case prg of
        Hosts{..}     -> out =<< runLocalDB selectHosts
        Apps{..}      -> out =<< runLocalDB (selectHostApplications argid')
        Items{..}     -> out =<< runLocalDB (selectAppItems argid')
        Trends{..}    -> out . (,,) asItem' asHost' =<< runRemoteDB (selectZabTrendItem argid')
        History{..}   -> out . sampled samples =<< runLocalDB (historyVectors $ selectHistory argid')
        Future{..}    -> out =<< runLocalDB (P.selectList [] [P.Asc ItemFutureId])
        Models{..}    -> out =<< runLocalDB (P.selectList [] [P.Asc FutureModelId])
        MigrateDb{..} -> runLocalDB $ E.runMigration migrateAll >> mapM_ P.insertUnique defaultMetricNames
        Dashboard{..} -> liftIO . BLC.putStrLn . encode =<< if cached then getDashboardCached else getDashboard dashboardConfig

        Th{..}        -> do
            when update $ runLocalDB $ updateThresholds (pack $ fromJust metric) lower warning high critical
            out =<< runLocalDB (selectThresholds $ fmap pack metric)

        Sync{..}      -> do
            when syncAll (populateZabbixParts >> populateDefaultFutures)
            case itemsToSync of
                [] -> populateAll >> executeFutures' Nothing
                is -> executeFutures' (Just $ map toSqlKey is)
            logInfoN "Now rebuilding the dashboard"
            void $ getDashboard dashboardConfig

        Configure{..}
            | not (null executable) -> do
                dir <- asks modelsDir
                let ex = dir ++ "/" ++ executable
                e   <- liftIO $ doesFileExist ex
                unless e (error $ "Executable " ++ ex ++ " not found")

                runLocalDB (P.insert_ $ FutureModel (pack executable))

            | item < 0 || model < 0 -> error "Provide either --executable or (ID and --model)"
            | otherwise             -> do
                fid <- newItemFuture (toSqlKey item) (toSqlKey model) False
                liftIO . putStrLn $ "Added future model id " ++ show (E.fromSqlKey fid)

#ifdef STATISTICS
        Compare{..} -> putStrLn =<< futureCompare argid' fromInterval toInterval
#else
        Compare{} -> error "habbix was not compiled with -fstatistics"
#endif

        Execute{..} -> do
            [(a, p, t, f, m)] <- getItemFutures $ Just [toSqlKey argid]
            let p' = if not (null params) then E.Value (encodeUtf8 $ pack params) else p
            r <- executeModel (a, p', t, f, m)
            case r of
                Right (_, r') -> out r'
                Left er       -> error er

getDashboard :: DashboardConfig -> Habbix Value
getDashboard config = runLocalDB $ do
    hosts <- selectHosts

    hostsJson <- fmap toJSON . forM hosts $ \(Entity hostId host) -> do
        items <- getItems hostId (hostName host)
        return $ object [ "hostid"   .= hostId
                        , "hostname" .= hostName host
                        , "items"    .= items ]

    time <- liftIO getCurrentTime

    let res = object [ "timestamp" .= show time, "hosts" .= hostsJson ]

    liftIO $ BLC.writeFile "dashboard.cached.json" (encode res)
    return res
  where
    getItems i k = let keys = fromMaybe (config M.! "default") $ M.lookup k config
                       in forM keys (selectItemDashboardData i)

getDashboardCached :: Habbix Value
getDashboardCached = do
    res <- liftIO $ BLC.readFile "dashboard.cached.json"
    return (fromJust $ decode res)

-- * Print

-- | How to output with 
class Out t where
    outJSON  :: t -> Value

    outHuman :: t -> BLC.ByteString
    outHuman = A.encodePretty . outJSON

    -- | Output SQL INSERT'S
    outSQL   :: t -> BLC.ByteString
    outSQL _ = "SQL output not implemented for this case"

instance Out [(Entity Metric, Entity Threshold)] where
    outJSON = toJSON . map p where
        p (Entity _ metric, Entity thresholdId threshold) = object
            [ "threshold" .= threshold, "thresholdId" .= thresholdId, "metric" .= metric ]

instance Out [Entity Host] where
    outJSON = toJSON . map p where
        p (Entity hid host) = object
            [ "hostid" .= hid
            , "host"   .= hostName host ]

instance Out [Entity Item] where
    outJSON = toJSON . map p where
        p (Entity iid item) = object
            [ "itemid"      .= iid
            , "hostid"      .= itemHost item
            , "key_"        .= itemKey_ item
            ,  "name"       .= itemName item
            , "description" .= itemDescription item
            ]

instance Out [Entity Application] where
    outJSON = toJSON . map p where
        p (Entity aid app) = object
                [ "hostid" .= applicationHost app
                , "appid"  .= aid
                , "name"   .= applicationName app
                ]

instance Out DP where
    outJSON (ts, vs) = toJSON $ zipWith (\t v -> (t, toJSON v)) (V.toList ts) (DV.toList vs)

-- | Print model info
instance Out [Entity FutureModel] where
    outJSON = toJSON . map p where
        p (Entity key model) = object [ "modelid" .= key
                                      , "name" .= futureModelName model
                                      ]

instance Out [Entity ItemFuture] where
    outJSON = toJSON . map p where
        p (Entity futId fut) = object
            [ "futureid" .= futId
            , "itemid"   .= itemFutureItem fut
            , "modelid"  .= itemFutureModel fut
            , "params"   .= fromMaybe (String "ERROR: params could not be parsed") (decodeStrict (itemFutureParams fut))
            ]

instance Out (Result Object) where
    outJSON Result{..} = toJSON $ zipWith (\c v -> object [ "time" .= c, "val" .= v]) (V.toList reClocks) (V.toList reValues)

instance Out (ItemId, HostId, (Entity Host, Entity Item, [Trend])) where
    outSQL (iid, hid, (Entity _ Host{..}, Entity _ Item{..}, trends)) = "BEGIN;"
        <> BLC.unlines (map BLC.pack
        [ printf "INSERT INTO hosts VALUES (%d, '%s', %d, %d, '%s');"
            (E.fromSqlKey hid) (unpack hostHost) hostStatus hostAvailable (unpack hostName)
        , printf "INSERT INTO items VALUES (%d, %d, %d, '%s', '%s', '%s', '%d');"
            (E.fromSqlKey iid) itemType (E.fromSqlKey hid) (unpack itemName)
            (unpack itemKey_) (unpack itemDescription) itemValueType
        , "INSERT INTO trend VALUES " ])
        <> BLC.intercalate ",\n" (map printfTrend trends) <> "; COMMIT;"
        where
            printfTrend Trend{..} = BLC.pack $
                printf " (%d, %d, %.4f, %.4f, %.4f)"
                (E.fromSqlKey iid) trendClock (tof trendValueMin) (tof trendValueAvg) (tof trendValueMax)
            tof :: Rational -> Double
            tof = fromRational
    outJSON (iid, hid, (Entity _ Host{..}, Entity _ Item{..}, trends)) = object
        [ "itemid" .= iid
        , "hostid" .= hid
        , "trends" .= map (\Trend{..} -> object ["clock" .= trendClock, "value_min" .= trendValueMin, "value_avg" .= trendValueAvg, "value_max" .= trendValueMax]) trends
        ]

-- * History stuff

-- | Sample of n evenly leaning towards end.
sampled :: Int -> DP -> DP
sampled n (ts, vs) =
    let interval   = fromIntegral (V.length ts) / fromIntegral n :: Double
        getIndex i = floor ((fromIntegral i + 1) * interval - 1)
        in ( V.generate n $ \i -> ts V.! getIndex i
           , DV.generate n $ \i -> vs DV.! getIndex i
           )
