{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Prelude hiding (print)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text.Format
import           Data.Text.Encoding (encodeUtf8)
import           Data.Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Database.Esqueleto
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
             | CSync    { outType :: DataOutType, syncAll :: Bool }
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

    , CSync     { syncAll = False &= help "Sync every table, not just history"
                } &= name "sync-db" &=  help "Synchronize remote db with local."

    ] &= program "habbix" &= verbosity

main :: IO ()
main = do
    prg         <- cmdArgs prgConf
    Just config <- Yaml.decodeFile "config.yaml"

    runHabbix (localDatabase config) (zabbixDatabase config) $ do

        let selHist = do item <- getJust (toSqlKey $ argid prg)
                         return . selectHistory $ Entity (toSqlKey $ argid prg) item

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
            CSync{..} | syncAll -> populateAll
                     | otherwise -> populateHistory

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
printHists (ts, vs) = zipWithM_ (\t v -> print "{} {}\n" (t, show v)) (U.toList ts) (V.toList vs)

-- | History data in JSON
printJsonHists :: (MonadIO m, ToJSON n) => Points n -> m ()
printJsonHists (ts, vs) =
    liftIO . BLC.putStrLn . encode . toJSON
    $ zipWith (\t v -> object [ "time" .= t, "val" .= v]) (U.toList ts) (V.toList vs)
    

-- History stuff

type Points n = (U.Vector Epoch, V.Vector n)

historyVectors :: DPS n -> DB (Points n)
historyVectors src = do
    xs <- src $$ CL.consume
    return (U.fromList (map fst xs), V.fromList (map snd xs))

-- | Sample of n evenly leaning towards end.
sampled :: Int -> Points n -> Points n
sampled n (ts, vs) =
    let interval = fromIntegral (U.length ts) / fromIntegral n
        in ( U.generate n $ \i -> ts U.! (floor $ (fromIntegral i + 1) * interval - 1)
           , V.generate n $ \i -> vs V.! (floor $ (fromIntegral i + 1) * interval - 1) )
