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
module Main where

import ZabbixDB (Epoch)
import Forecast
import Future

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import qualified Data.Vector.Storable as V
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy.Char8 as C
import Numeric.Statistics

type Predict = State (V.Vector Epoch, V.Vector Double)

data Filter = DailyMax | DailyMin

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

    -- filters
    let (_, (clocks, values)) = (`runState` (V.convert evClocks, V.convert evValues)) $ do
            withMaybe preFilter applyFilter 

    -- lin reg
    let (a, b, r2) = simpleLinearRegression (V.convert $ V.map fromIntegral clocks) (V.convert values)

    -- results
    let details = Details { r2det     = r2 }
        res     = Result  { reClocks  = DV.iterateN 7 (+ aday) (DV.last evClocks)
                          , reValues  = (\x -> a * fromIntegral x + b) <$> reClocks res
                          , reDetails = details }
    C.putStrLn (encode res)

-- | apply the filter
applyFilter :: Filter -> Predict ()
applyFilter fi = do
    clocks <- gets fst :: Predict (V.Vector Epoch)
    let (cmin, cmax) = (floor $ fromIntegral (V.minimum clocks) / fromIntegral aday, ceiling $ fromIntegral (V.maximum clocks) / fromIntegral aday)
        ixs          = cut (V.map fromIntegral clocks) $ V.map (fromIntegral . (* aday)) $ V.fromList [cmin .. cmax]

    modify (indexedFilter ixs fi *** indexedFilter ixs fi)

indexedFilter :: (Ord n, V.Storable n) => V.Vector Int -> Filter -> V.Vector n -> V.Vector n
indexedFilter ixs DailyMax xs = V.zipWith (\i j -> V.maximum $ V.slice i j xs) ixs (V.init ixs)
indexedFilter ixs DailyMin xs = V.zipWith (\i j -> V.minimum $ V.slice i j xs) ixs (V.init ixs)

aday :: Int
aday = 60 * 60 * 24
