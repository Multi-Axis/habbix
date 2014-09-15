{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Main (main.hs)
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main where

import ZabbixDB
import Query

import Prelude hiding (print)
import Data.Text.Format
import Database.Esqueleto
import Control.Monad.IO.Class
import Control.Monad
import System.Environment

localConn  = "host=localhost port=5432 user=zabbix dbname=zabbix_db password=pass"
remoteConn = "host=localhost port=5432 user=zabbix dbname=zabbix_db password=pass"

-- | Usage string for the cli program.
usage :: String
usage = unlines
    [ "hosts            List all hosts and groups (except templates)"
    , "apps <hostid>    List available \"metric groups\" for the host"
    , "items <appid>    List available \"metrics\" in the metric group <appid>"
    , "history <itemid> Print out <time>,<value> for the given metric <itemid>"
    ]

main :: IO ()
main = runHabbix localConn remoteConn $ do
    (cmd : args) <- liftIO getArgs
    let param = read (head args) :: Int64
    case cmd of
        "hosts" -> runLocalDB selectHosts                               >>= liftIO . printHosts
        "apps"  -> runLocalDB (selectHostApplications $ toSqlKey param) >>= liftIO . printApps
        "items" -> runLocalDB (selectAppItems         $ toSqlKey param) >>= liftIO . printItems
        "hist"  -> runLocalDB (selectHistory          $ toSqlKey param) >>= liftIO . printHists
        _ -> liftIO $ putStrLn usage

-- | Print host info
printHosts :: [(Entity Group, Entity Host)] -> IO ()
printHosts hosts = do
    putStrLn "Host ID |           Group           | Name"
    forM_ hosts $ \(Entity _ gp, Entity hid host) ->
        print "{} | {} | {}\n" (left 7 ' ' $ fromSqlKey hid, right 25 ' ' $ groupName gp, hostName host)

-- | Print single item id and name
printItems :: [Entity Item] -> IO ()
printItems items = do
    putStrLn "Item ID | Description"
    forM_ items $ \(Entity iid item) ->
        print "{} | {}\n" (left 7 ' ' $ fromSqlKey iid, itemDescription item)

-- | Print a history item <epoch>,<value>
printHists :: [(Epoch, FixedE4)] -> IO ()
printHists = mapM_ (\(t,v) -> print "{},{}\n" (t, show v))

-- | Print an application (metric group)
printApps :: [Entity Application] -> IO ()
printApps apps = do
    putStrLn " App ID | Name"
    forM_ apps $ \(Entity aid app) ->
        print "{} | {}\n" (left 7 ' ' $ fromSqlKey aid, applicationName app)
