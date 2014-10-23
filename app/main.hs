{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Main where

import ZabbixDB
import Query
import Future

import           Prelude hiding (print)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text (pack)
import           Data.Text.Format
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as V
import           Database.Esqueleto
import qualified Database.Persist as P
import           System.Console.CmdArgs

data Config = Config
            { localDatabase :: ConnectionString
            , zabbixDatabase :: ConnectionString
            }

instance FromJSON Config where
    parseJSON (Object o) = Config <$> (encodeUtf8 <$> o .: "localDatabase")
                                  <*> (encodeUtf8 <$> o .: "zabbixDatabase")
    parseJSON _          = mzero

data Program = CLsHosts { outType :: DataOutType }
             | CLsApps  { outType :: DataOutType, argid :: Int64 }
             | CLsItems { outType :: DataOutType, argid :: Int64 }
             | CLsData  { outType :: DataOutType, argid :: Int64, samples :: Int }
             | CMigrate { outType :: DataOutType }
             | CSync    { outType :: DataOutType, syncAll :: Bool, itemsToSync :: [Int64] }
             | CLsModels { outType :: DataOutType }
             | CLsFuture { outType :: DataOutType }
             | Compare   { outType :: DataOutType, argid :: Int64, fromInterval :: (Epoch, Epoch), toInterval :: (Epoch, Epoch) }
             | CAddModel { outType :: DataOutType, executable :: String }
             | NewFuture { outType :: DataOutType, argid :: Int64, model :: Int64 }
             deriving (Show, Data, Typeable)

data DataOutType = OutHuman | OutJSON deriving (Show, Data, Typeable)

prgConf :: Program
prgConf = modes
    [ CLsHosts  { outType = enum [ OutHuman &= name "human" &= help "Human-readable output"
                                 , OutJSON  &= name "json"  &= help "JSON output"
                                 ]
                } &= name "ls-hosts"
                  &= help "List all hosts and groups except templates"
    , CLsApps   { argid = def &= opt (-1 :: Int64) &= argPos 0 &= typ "ID"
                } &= name "ls-apps"
                  &= help "List available \"metric groups\" for the Host ID"
    , CLsItems  { } &= name "ls-items"
                    &= help "List available \"metrics\" in the metric group App ID>"
    , CLsData   { samples = 80 &= help "Sample resolution (default 80)"
                } &= name "ls-data" &= help "Print history data for <itemid>"

    , CMigrate  { } &= name "migrate-db" &= help "Create or update the local DB schema"

    , CSync     { syncAll     = False &= help "Sync every table, not just history"
                , itemsToSync = [] &= help "Optional item_future.id's to sync"
                } &= name "sync-db" &=  help "Synchronize remote db with local and run futures"

    , CLsModels { } &= name "ls-models" &= help "List available future models"

    , CLsFuture { } &= name "ls-future" &= help "List all item futures"

    , CAddModel { executable = def &= argPos 0 &= typ "FILENAME"
                } &= name "add-model" &= help "Register a model named FILENAME in forecast_models"

    , NewFuture { model = def &= argPos 1 &= typ "MODELID"
                } &= name "new-future" &= help "Add future with MODELID for ITEMID"

    , Compare   { fromInterval = def &= help "Interval to use with predictions"
                , toInterval   = def &= help "Interval to compare the predicted model to"
                } &= help "Compare predictions from knowing A to an actual history B"

    ] &= program "habbix" &= verbosity

main :: IO ()
main = do
    Just config <- Yaml.decodeFile "config.yaml"
    prg         <- cmdArgs prgConf
    debugSql    <- isLoud

    runHabbix debugSql (localDatabase config) (zabbixDatabase config) $ do

        let selHist = do item <- getJust (toSqlKey $ argid prg)
                         return $ selectHistory (toSqlKey $ argid prg) (itemValueType item)

        case prg of
            CLsHosts{..} -> runLocalDB selectHosts >>= liftIO . printHosts
            CLsApps{..}  -> runLocalDB (selectHostApplications $ toSqlKey argid) >>= liftIO . printApps
            CLsItems{..} -> runLocalDB (selectAppItems $ toSqlKey argid) >>= liftIO . printItems

            CLsData{..}  ->
                let output :: (Show n, ToJSON n, Num n) => Points n -> DB ()
                    output = case outType of
                        OutHuman -> printHists . sampled samples
                        OutJSON  -> printJsonHists . sampled samples
                    in runLocalDB $ selHist >>= either (historyVectors >=> output) (historyVectors >=> output)

            CMigrate{..} -> runLocalDB (runMigration migrateAll)

            CSync{..} -> do
                when syncAll populateZabbixParts
                case itemsToSync of
                    [] -> populateHistory >> executeFutures Nothing
                    is -> executeFutures $ Just $ map toSqlKey is

            CLsModels{..} -> runLocalDB (P.selectList [] []) >>= liftIO . printFutureModels

            CLsFuture{..} -> runLocalDB (P.selectList [] []) >>= liftIO . printItemFutures

            CAddModel{..} -> runLocalDB $ P.insert_ $ FutureModel (pack executable)

            NewFuture{..} -> runLocalDB $ P.insert_ $ ItemFuture (toSqlKey argid) (toSqlKey model) "{}"

#ifdef STATISTICS
            Compare{..} | argid <= 0 -> error "itemFutureId must be > 0"
                        | otherwise  -> futureCompare (toSqlKey argid) fromInterval toInterval
#endif

-- | Print model info
printFutureModels :: [Entity FutureModel] -> IO ()
printFutureModels models = do
    putStrLn " ID  | Name"
    forM_ models $ \(Entity key model) ->
        print "{} | {}\n" (fromSqlKey key, futureModelName model)

printItemFutures :: [Entity ItemFuture] -> IO ()
printItemFutures futs = do
    putStrLn " ItemId | modelId | params "
    forM_ futs $ \(Entity _ fut) ->
        print "{} | {} | {}\n"
            ( fromSqlKey $ itemFutureItem fut
            , fromSqlKey $ itemFutureModel fut
            , decodeUtf8 $ itemFutureParams fut
            )

-- | Print host info
printHosts :: [(Entity Group, Entity Host)] -> IO ()
printHosts hosts = do
    putStrLn " HostID |           Group           | Name"
    forM_ hosts $ \(Entity _ gp, Entity hid host) ->
        print "{} | {} | {}\n" (left 7 ' ' $ fromSqlKey hid, right 25 ' ' $ groupName gp, hostName host)

-- | Print single item id and name
printItems :: [Entity Item] -> IO ()
printItems items = do
    putStrLn "ItemID | HostID |                key_                 |             Name              | Description"
    forM_ items $ \(Entity iid item) -> print "{} | {} | {} | {} | {}\n"
        ( left 5 ' ' $ fromSqlKey iid
        , left 7 ' ' $ fromSqlKey (itemHost item)
        , left 35 ' ' $ itemKey_ item
        , left 25 ' ' $ itemName item
        , itemDescription item
        )

-- | Print an application (metric group)
printApps :: [Entity Application] -> IO ()
printApps apps = do
    putStrLn " HostID | AppID | Name"
    forM_ apps $ \(Entity aid app) -> print "{} | {} | {}\n"
            ( left 6 ' ' $ fromSqlKey $ applicationHost app
            , left 6 ' ' $ fromSqlKey aid
            , applicationName app)

-- | Print a history item <epoch>,<value>
printHists :: (MonadIO m, Show n) => Points n -> m ()
printHists (ts, vs) = zipWithM_ (\t v -> print "{} {}\n" (t, show v)) (V.toList ts) (DV.toList vs)

-- | History data in JSON
printJsonHists :: (MonadIO m, ToJSON n) => Points n -> m ()
printJsonHists (ts, vs) =
    liftIO . BLC.putStrLn . encode . toJSON
    $ zipWith (\t v -> object [ "time" .= t, "val" .= v]) (V.toList ts) (DV.toList vs)
    

-- History stuff

-- | Sample of n evenly leaning towards end.
sampled :: Int -> Points n -> Points n
sampled n (ts, vs) =
    let interval   = fromIntegral (V.length ts) / fromIntegral n :: Double
        getIndex i = floor ((fromIntegral i + 1) * interval - 1)
        in ( V.generate n $ \i -> ts V.! getIndex i
           , DV.generate n $ \i -> vs DV.! getIndex i
           )
