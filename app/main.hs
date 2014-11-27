{-# LANGUAGE RecordWildCards #-}
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
import           Data.Aeson hiding (Result)
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe
import           Data.Text          (pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
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
    [ Metric "mem" "system.stat[memory,fre]"
    , Metric "cpu" "system.cpu.load[percpu,avg5]"
    ]

data Config = Config
            { localDatabase :: ConnectionString
            , zabbixDatabase :: ConnectionString
            , modeldir :: FilePath
            }

instance FromJSON Config where
    parseJSON (Object o) = Config <$> (encodeUtf8 <$> o .: "localDatabase")
                                  <*> (encodeUtf8 <$> o .: "zabbixDatabase")
                                  <*> (fromMaybe "forecast_models" <$> o .:? "modeldir")
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
        Sync{..}      -> do
            when syncAll (populateZabbixParts >> populateDefaultFutures)
            case itemsToSync of
                [] -> populateAll >> executeFutures' Nothing
                is -> executeFutures' (Just $ map toSqlKey is)
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
            r <- executeModelNextWeek (a, p', t, f, m)
            case r of
                Right (_, r') -> out r'
                Left er       -> error er

-- * Print

-- | How to output with 
class Out t where
    outJSON  :: t -> Value

    outHuman :: t -> BLC.ByteString
    outHuman = A.encodePretty . outJSON

    -- | Output SQL INSERT'S
    outSQL   :: t -> BLC.ByteString -- TODO
    outSQL _ = "SQL output not implemented for this case"

instance Out [(Entity Group, Entity Host)] where
    outJSON = toJSON . map p where
        p (Entity _ gp, Entity hid host) = object
            [ "hostid" .= hid
            , "group"  .= groupName gp
            , "host"   .= hostName host
            ]

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
