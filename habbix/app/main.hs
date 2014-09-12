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

import Database.Esqueleto
import Control.Monad.IO.Class

localConn  = "host=localhost port=5432 user=zabbix dbname=zabbix_db password=pass"
remoteConn = "host=localhost port=5432 user=zabbix dbname=zabbix_db password=pass"

main :: IO ()
main = runHabbix localConn remoteConn $ do
    items <- runLocalDB selectItems
    hists <- runLocalDB $ selectHistory (fromSqlKey $ entityKey $ head items)
    liftIO $ mapM_ print hists
