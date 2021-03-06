{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
------------------------------------------------------------------------------
-- | 
-- Module         : ZabbixDB
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- 'Habbix' is the context in which we operate. Provides @IO@ and two
-- database connections via 'runLocalDB' and `runRemoteDB'.
------------------------------------------------------------------------------
module ZabbixDB
    ( Habbix, runHabbix, ConnectionString

    -- * DB
    , DB, runLocalDB, runRemoteDB, module Models, module ZabbixModels
    , modelsDir

    -- * Utility
    , Epoch, tshow, asks
    ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Base
import Database.Persist.Postgresql
import System.Log.FastLogger

import Models
import ZabbixModels

data HabbixState = HabbixState
                 { localPool :: ConnectionPool
                 , remotePool :: ConnectionPool
                 , modelsDir :: FilePath
                 }

newtype Habbix a = Habbix { unHabbix :: ResourceT (ReaderT HabbixState (LoggingT IO)) a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadResource, MonadReader HabbixState, MonadLogger)

-- This is just a mechanical MonadBaseControl instance, albeit rather
-- tricky to grasp. Without the associated type ghc would derive this too :)
instance MonadBaseControl IO Habbix where
#if MIN_VERSION_monad_control(1,0,0)
    type StM Habbix a = NewStM a
    liftBaseWith f = Habbix (liftBaseWith (\run -> f (liftM StMHabbix . run . unHabbix)))
    restoreM = Habbix . restoreM . unStMHabbix

newtype NewStM a = StMHabbix { unStMHabbix :: StM (ResourceT (ReaderT HabbixState (LoggingT IO))) a }

#else
    newtype StM Habbix a = StMHabbix { unStMHabbix :: StM (ResourceT (ReaderT HabbixState (LoggingT IO))) a }
    liftBaseWith f = Habbix (liftBaseWith (\run -> f (liftM StMHabbix . run . unHabbix)))
    restoreM = Habbix . restoreM . unStMHabbix
#endif

type DB = SqlPersistT Habbix

type Epoch = Int

-- | Execute a transaction in the local DB.
runLocalDB :: DB a -> Habbix a
runLocalDB m = asks localPool >>= runSqlPool m

-- | Execute a transaction in the remote DB.
runRemoteDB :: SqlPersistT Habbix a -> Habbix a
runRemoteDB m = asks remotePool >>= runSqlPool m

-- | @runHabbix debugsql local remote action@
runHabbix :: Bool -> Bool -> FilePath -> ConnectionString -> ConnectionString -> Habbix a -> IO a
runHabbix isquiet isloud modelfp localConn remoteConn ma = do
    ls <- newStderrLoggerSet defaultBufSize
    let myLog _loc src level msg
            | level == LevelDebug && not isloud = return ()
            | level == LevelInfo  && isquiet    = return ()
            | level == LevelWarn  && isquiet    = return ()
            | otherwise                         = pushLogStr ls $
                "[" <> toLogStr (drop 5 (show level))
                    <> (if T.null src then mempty else "#" <> toLogStr src)
                    <>  "] " <> msg <> "\n"

    (`runLoggingT` myLog) $
        withPostgresqlPool localConn 10 $ \lpool ->
        withPostgresqlPool remoteConn 10 $ \rpool ->
        runReaderT (runResourceT $ unHabbix ma) (HabbixState lpool rpool modelfp)

tshow :: Show a => a -> T.Text
tshow = T.pack . show
