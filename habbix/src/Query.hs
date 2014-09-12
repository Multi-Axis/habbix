------------------------------------------------------------------------------
-- | 
-- Module         : Query
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Query where

import ZabbixDB

import Control.Monad
import Control.Arrow
import Database.Esqueleto
import qualified Database.Persist as P

-- | Get all history for given item id.
selectHistory :: Int64 -> DB [(Epoch, FixedE4)]
selectHistory itemid = liftM (map (unValue *** unValue)) . select . from $ \history -> do
    where_ $ history ^. HistoryItemid ==. val itemid
    orderBy [asc $ history ^. HistoryClock]
    return (history ^. HistoryClock, history ^. HistoryValue)

selectItems :: DB [Entity Item]
selectItems = P.selectList [] [P.Asc ItemName]
