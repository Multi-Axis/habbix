{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
------------------------------------------------------------------------------
-- | 
-- Module         : ZabbixDB
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module ZabbixDB
    ( module ZabbixDB
    , module Models
    , ConnectionString
    ) where

import Control.Applicative
import Data.Int as ZabbixDB
import FixedE4 as ZabbixDB

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Base
import Database.Persist.Postgresql

import Models

data HabbixState = HabbixState
                 { localPool :: ConnectionPool
                 , remotePool :: ConnectionPool
                 }

newtype Habbix a = Habbix { unHabbix :: ResourceT (ReaderT HabbixState (LoggingT IO)) a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadResource, MonadReader HabbixState, MonadLogger)

instance MonadBaseControl IO Habbix where
    newtype StM Habbix a = StMHabbix { unStMHabbix :: StM (ResourceT (ReaderT HabbixState (LoggingT IO))) a }
    liftBaseWith f = Habbix (liftBaseWith (\run -> f (liftM StMHabbix . run . unHabbix)))
    restoreM = Habbix . restoreM . unStMHabbix

type DB = SqlPersistT Habbix

type Epoch = Int

-- | Execute a transaction in the local DB.
runLocalDB :: DB a -> Habbix a
runLocalDB m = asks localPool >>= runSqlPool m

-- | Execute a transaction in the remote DB.
runRemoteDB :: SqlPersistT Habbix a -> Habbix a
runRemoteDB m = asks remotePool >>= runSqlPool m

runHabbix :: ConnectionString -> ConnectionString -> Habbix a -> IO a
runHabbix localConn remoteConn ma =
    runStderrLoggingT $
    withPostgresqlPool localConn 10 $ \lpool ->
    withPostgresqlPool remoteConn 10 $ \rpool ->
        runReaderT (runResourceT $ unHabbix ma) (HabbixState lpool rpool)

-- runMigration migrateAll
