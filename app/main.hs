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
module Main (main) where

import           ZabbixDB hiding (Future, History)
import           Query
import           Future

import           Prelude
import           Control.Applicative
import           Control.Monad
import           Data.Aeson hiding (Result)
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector                as DV
import qualified Data.Vector.Storable       as V
import qualified Data.Yaml                  as Yaml
import qualified Database.Esqueleto         as E
import           Database.Esqueleto              (toSqlKey, Entity(..))
import qualified Database.Persist           as P
import           System.Console.CmdArgs

data Config = Config
            { localDatabase :: ConnectionString
            , zabbixDatabase :: ConnectionString
            }

instance FromJSON Config where
    parseJSON (Object o) = Config <$> (encodeUtf8 <$> o .: "localDatabase")
                                  <*> (encodeUtf8 <$> o .: "zabbixDatabase")
    parseJSON _          = mzero

data Program = Hosts     { config :: String, outType :: DataOutType }
             | Apps      { config :: String, outType :: DataOutType, argid :: Int64 }
             | Items     { config :: String, outType :: DataOutType, argid :: Int64 }
             | History   { config :: String, outType :: DataOutType, argid :: Int64, samples :: Int }
             | Future    { config :: String, outType :: DataOutType }
             | Models    { config :: String, outType :: DataOutType }
             | MigrateDb { config :: String, outType :: DataOutType }
             | Sync      { config :: String, outType :: DataOutType, syncAll :: Bool, itemsToSync :: [Int64] }
             | Configure { config :: String, outType :: DataOutType, item :: Int64, model :: Int64, executable :: String }
             | Execute   { config :: String, outType :: DataOutType, argid :: Int64, params :: String, outCombine :: Bool }
             | Compare   { config :: String, outType :: DataOutType, argid :: Int64, fromInterval :: (Epoch, Epoch), toInterval :: (Epoch, Epoch) }
             deriving (Show, Data, Typeable)

data DataOutType = OutHuman | OutJSON deriving (Eq, Show, Data, Typeable)

prgConf :: Program
prgConf = modes
    -- Query only
    [ Hosts     { outType = enum [ OutHuman &= name "human" &= help "Human-readable JSON output"
                                 , OutJSON  &= name "json"  &= help "Bare JSON output"
                                 ]
                , config  = "config.yaml" &= typFile &= help "yaml config file (default: ./config.yaml)"
                } &= help "List all hosts and groups except templates"
    , Apps      { argid   = (-1) &= argPos 0 &= typ "ID"
                } &= help "List available \"metric groups\" for the Host ID"
    , Items     { } &= help "List available \"metrics\" in the metric group App ID>"
    , History   { samples = 80 &= help "Sample resolution (default 80)"
                } &= help "Print history data for <itemid>"
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
    prg       <- cmdArgs prgConf
    Just conf <- Yaml.decodeFile (config prg)
    debugSql  <- isLoud

    let selHist = do item <- E.getJust (toSqlKey $ argid prg)
                     return $ selectHistory (toSqlKey $ argid prg) (itemValueType item)

    out <- runHabbix debugSql (localDatabase conf) (zabbixDatabase conf) $ case prg of
        Hosts{..}   -> printHosts <$> runLocalDB selectHosts
        Apps{..}    -> printApps  <$> runLocalDB (selectHostApplications $ toSqlKey argid)
        Items{..}   -> printItems <$> runLocalDB (selectAppItems $ toSqlKey argid)
        History{..}
            | argid < 0 -> error "ItemID must be >= 0"
            | otherwise -> runLocalDB $ selHist >>= either (historyVectors >=> return . printHists . sampled samples)
                                                           (historyVectors >=> return . printHists . sampled samples)

        Future{..}  -> printItemFutures  <$> runLocalDB (P.selectList [] [])
        Models{..}  -> printFutureModels <$> runLocalDB (P.selectList [] [])

        MigrateDb{..} -> runLocalDB (E.runMigration migrateAll) >> return (String "done")
        Sync{..}      -> do
            when syncAll populateZabbixParts
            case itemsToSync of
                [] -> populateHistory >> executeFutures' Nothing >> return (String "done")
                is -> executeFutures' (Just $ map toSqlKey is)   >> return (String "done")

        Configure{..}
            | not (null executable) -> do
                -- TODO: check executable exists, too
                runLocalDB (P.insert_ $ FutureModel (pack executable))
                return $ String "done"

            | item < 0 || model < 0 -> error "Provide either --executable or (ID and --model)"
            | otherwise             ->
                runLocalDB (P.insert_ $ ItemFuture (toSqlKey item) (toSqlKey model) "{}" "{}")
                >> return (String "done")

        Compare{..}
            | argid < 0 -> error "itemFutureId must be >= 0"
            | otherwise -> String . pack <$> futureCompare (toSqlKey argid) fromInterval toInterval

        Execute{..}
            | argid < 0 -> error "ID must be >= 0"
            | otherwise -> do
                [(a, p, t, f, m)] <- getItemFutures $ Just [toSqlKey argid]
                let p' = if not (null params) then E.Value (encodeUtf8 $ pack params) else p
                (if outCombine then resultCombine else toJSON) . snd
                    <$> executeModelNextWeek (a, p', t, f, m)

    BLC.putStrLn $ (if outType prg == OutJSON then encode else A.encodePretty) out

-- * Print

-- | Print host info
printHosts :: [(Entity Group, Entity Host)] -> Value
printHosts = toJSON . map p
  where
    p (Entity _ gp, Entity hid host) = object
        [ "hostid" .= hid
        , "group"  .= groupName gp
        , "host"   .= hostName host
        ]

-- | Print single item id and name
printItems :: [Entity Item] -> Value
printItems = toJSON . map p
  where
    p (Entity iid item) = object
        [ "itemid"      .= iid
        , "hostid"      .= itemHost item
        , "key_"        .= itemKey_ item
        ,  "name"       .= itemName item
        , "description" .= itemDescription item
        ]

-- | Print an application (metric group)
printApps :: [Entity Application] -> Value
printApps = toJSON . map p
  where
    p (Entity aid app) = object
            [ "hostid" .= applicationHost app
            , "appid"  .= aid
            , "name"   .= applicationName app
            ]

-- | Print a history item <epoch>,<value>
printHists :: ToJSON n => Points n -> Value
printHists (ts, vs) = toJSON $ zipWith (\t v -> (t, toJSON v)) (V.toList ts) (DV.toList vs)

-- | Print model info
printFutureModels :: [Entity FutureModel] -> Value
printFutureModels = toJSON . map p
  where
    p (Entity key model) = object [ "modelid" .= key
                                  , "name" .= futureModelName model
                                  ]

printItemFutures :: [Entity ItemFuture] -> Value
printItemFutures = toJSON . map p
  where
    p (Entity futId fut) = object
        [ "futureid" .= futId
        , "itemid"   .= itemFutureItem fut
        , "modelid"  .= itemFutureModel fut
        , "params"   .= fromMaybe (String "ERROR: params could not be parsed") (decodeStrict (itemFutureParams fut))
        ]

resultCombine :: Result Object -> Value
resultCombine Result{..} = toJSON
    $ zipWith (\c v -> object [ "clock" .= c, "value" .= v]) (V.toList reClocks) (V.toList reValues)

-- * History stuff

-- | Sample of n evenly leaning towards end.
sampled :: Int -> Points n -> Points n
sampled n (ts, vs) =
    let interval   = fromIntegral (V.length ts) / fromIntegral n :: Double
        getIndex i = floor ((fromIntegral i + 1) * interval - 1)
        in ( V.generate n $ \i -> ts V.! getIndex i
           , DV.generate n $ \i -> vs DV.! getIndex i
           )
