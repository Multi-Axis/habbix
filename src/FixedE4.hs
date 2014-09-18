------------------------------------------------------------------------------
-- | 
-- Module         : FixedE4
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module FixedE4 where

import Data.Fixed

data E4 = E4
type FixedE4 = Fixed E4
instance HasResolution E4 where
    resolution _ = 4
