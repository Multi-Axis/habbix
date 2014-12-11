{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Forecast
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Regression utilities.
------------------------------------------------------------------------------
module Forecast where

import ZabbixDB (Epoch)

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Char (toLower)
import Data.Function
import Data.Aeson.TH
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector as DV
import           Data.Text (pack, unpack)
import           Database.Persist.Quasi (PersistSettings(psToDBName), lowerCaseSettings)

-- | A simple stateful abstraction
type Predict = StateT (V.Vector Epoch, V.Vector Double) IO

-- | `simpleLinearRegression xs ys` gives (a, b, r2) for the line
-- y = a * x + b.
simpleLinearRegression :: (Eq n, Fractional n, V.Storable n) => Vector n -> Vector n
                       -> Maybe (n, n, n)
simpleLinearRegression xs ys
    | num_x == 0 || num_y == 0 = Nothing
    | otherwise                = Just (a, b, r2)
    where
        a = cov_xy / var_x
        b = mean_y - a * mean_x

        cov_xy = (V.sum (V.zipWith (*) xs ys) / num_x) - mean_x * mean_y
        var_x  = V.sum (V.map (\x -> (x - mean_x) ^ (2 :: Int)) xs) / num_x

        mean_x = V.sum xs / num_x
        mean_y = V.sum ys / num_y

        num_x = fromIntegral (V.length xs)
        num_y = fromIntegral (V.length ys)

        r2     = 1 - ss_res / ss_tot
        ss_tot = V.sum $ V.map (\y -> (y - mean_y) ^ (2 :: Int)) ys
        ss_res = V.sum $ V.zipWith (\x y -> (y - f x) ^ (2 :: Int)) xs ys
        f x    = a * x + b

-- |
--
-- @
-- y - y0 = a * (x - x0)
--      ==> y = a * x + (y0 - a * x0) = a * x + b'
-- @
drawFuture :: Double -- ^ a
           -> Double -- ^ b
           -> Maybe (Epoch, Double) -- ^ draw starting at (time, value)
           -> V.Vector Epoch -- ^ clocks
           -> V.Vector Double
drawFuture a b mlast = V.map (\x -> a * fromIntegral x + b')
    where b' = case mlast of
                   Just (x0, y0) -> y0 - a * fromIntegral x0
                   Nothing       -> b

-- * Filters

data Filter = Filter
            { aggregate      :: FilterAggregate
            , interval       :: Epoch -- ^ seconds; a day an hour...
            , intervalStarts :: Epoch }

data FilterAggregate = Max | Min | Avg

applyFilter :: Filter -> Predict ()
applyFilter Filter{..} = do
    (clocks, values) <- get

    let ixs       = DV.map fst $ splittedAt (intervalStarts `rem` interval) interval (V.convert clocks)
        slices vs = DV.zipWith (\i j -> DV.slice i (j - i) vs) ixs (DV.tail ixs)

    put . (V.convert *** V.convert)
        . DV.unzip . DV.map (apply aggregate) . slices
        $ DV.zip (V.convert clocks) (V.convert values)

apply aggregate = case aggregate of
   Max -> DV.maximumBy (compare `on` snd)
   Min -> DV.minimumBy (compare `on` snd)
   Avg -> (vectorMedian *** vectorAvg) . DV.unzip

-- | Fold left; accumulate the current index (and timestamp) when day changes.
splittedAt :: Epoch -> Epoch -> DV.Vector Epoch -> DV.Vector (Int, Epoch)
splittedAt start interval =
    DV.ifoldl' go <$> DV.singleton . ((0,) . toInterval) . DV.head <*> DV.init
  where
      go v m c | (_, t) <- DV.last v, t == toInterval c = v
               | otherwise                              = v `DV.snoc` (m, toInterval c)

      toInterval   = floor . (/ fromIntegral interval) . fromIntegral . (+ start)
      maybeDrop vs =
          let l = DV.length vs - 1 in
          (if snd (vs DV.! 0)       + (interval `div` 2) > snd (vs DV.! 1) then DV.tail else id)
          . (if snd (vs DV.! (l - 1)) + (interval `div` 2) > snd (vs DV.! l) then DV.init else id)
          $ vs


-- * Utility

vectorAvg :: (Real a, Fractional a) => DV.Vector a -> a
vectorAvg v = fromRational $ toRational (DV.sum v) / toRational (DV.length v)

vectorMedian :: DV.Vector a -> a
vectorMedian v = v DV.! floor (fromIntegral (DV.length v) / 2 :: Double)

aesonOptions = defaultOptions
    { fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack
    , constructorTagModifier = map toLower }
