{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- | 
-- Module         : ZabbixModels
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module ZabbixModels where

import Database.Persist.TH
import Models

-- Below: only used when fetching from zabbix. (Always empty in local db)

share [mkPersist sqlSettings] [persistLowerCase|
ZabHist sql=history
    item ItemId        sql=itemid
    clock Int
    value Rational
ZabHistUint sql=history_uint
    item  ItemId       sql=itemid
    clock Int
    value Rational
ZabTrend sql=trends
    item ItemId        sql=itemid
    clock Int
    valueMin  Rational
    valueAvg  Rational
    valueMax  Rational
ZabTrendUint sql=trends_uint
    item  ItemId       sql=itemid
    clock Int
    valueMin  Rational
    valueAvg  Rational
    valueMax  Rational
|]
