{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Main (linreg.hs)
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main (main) where

import ZabbixDB (Epoch)
import Forecast
import Future

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Function
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Vector.Storable as V
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy.Char8 as C

type Predict = State (V.Vector Epoch, V.Vector Double)

data Filter = DailyMax | DailyMin | DailyAvg

data Params = Params
            { preFilter :: Maybe Filter }

data Details = Details { r2det :: Double }

$(deriveJSON defaultOptions ''Filter)
$(deriveJSON defaultOptions ''Params)
$(deriveJSON defaultOptions ''Details)

main :: IO ()
main = do
    Just Event{..} <- decode <$> C.getContents
    let Params{..} = evParams

    let (res, _) = (`runState` (V.convert evClocks, V.convert evValues)) $ do

            -- PreFilters
            withMaybe preFilter applyFilter 

            -- lin reg
            (a, b, r2) <- uncurry simpleLinearRegression . (V.convert . V.map fromIntegral *** V.convert) <$> get

            let details = Details { r2det = r2 }

            return Result { reClocks  = evDrawFuture
                          , reValues  = V.map (\x -> a * fromIntegral x + b) evDrawFuture
                          , reDetails = details }
    C.putStrLn (encode res)

-- | apply the filter
applyFilter :: Filter -> Predict ()
applyFilter fi = do
    (clocks, values) <- get

    let ixs       = DV.map fst $ filterDaily (V.convert clocks)
        slices v  = DV.zipWith (\i j -> DV.slice i (j - i) v) ixs (DV.init ixs)

    put . (V.convert *** V.convert) . DV.unzip . DV.map apply . slices
        $ DV.zip (V.convert clocks) (V.convert values)

  where
    apply = case fi of
       DailyMax -> DV.maximumBy (compare `on` snd)
       DailyMin -> DV.minimumBy (compare `on` snd)
       DailyAvg -> (vectorMedian *** vectorAvg) . DV.unzip

-- | Fold left; accumulate the current index (and timestamp) when day changes.
filterDaily :: DV.Vector Epoch -> DV.Vector (Int, Epoch)
filterDaily = DV.ifoldl' go <$> DV.singleton . (\x -> (0, x)) . DV.head <*> DV.init
    where
        go v m c
            | (_, t) <- DV.last v, t /= toDay c = v
            | otherwise                         = v `DV.snoc` (m, toDay c)

vectorAvg :: (Real a, Fractional a) => DV.Vector a -> a
vectorAvg v = fromRational $ toRational (DV.sum v) / toRational (DV.length v)

vectorMedian :: DV.Vector a -> a
vectorMedian v = v DV.! floor (fromIntegral (DV.length v) / 2 :: Double)

toDay :: Epoch -> Int
toDay = floor . (/ fromIntegral aday) . fromIntegral

aday :: Int
aday = 60 * 60 * 24
