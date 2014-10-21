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

    let (res, _) = (`runState` (V.convert evClocks, V.convert evValues)) $ do

            -- PreFilters
            withMaybe preFilter applyFilter 

            -- lin reg
            (a, b, r2) <- uncurry simpleLinearRegression . (V.convert . V.map fromIntegral *** V.convert) <$> get

            let details = Details { r2det = r2 }

            return $ Result { reClocks  = DV.iterateN (ceiling $ fromIntegral (snd evDrawFuture - fst evDrawFuture) / fromIntegral aday) (+ aday) (fst evDrawFuture)
                            , reValues  = (\x -> a * fromIntegral x + b) <$> reClocks res
                            , reDetails = details }
    C.putStrLn (encode res)

-- | apply the filter
applyFilter :: Filter -> Predict ()
applyFilter fi = do
    (clocks, values) <- get
    let zipped = DV.zip (V.convert clocks) (V.convert values)
    put $ (V.convert *** V.convert) $ DV.unzip $ DV.foldl' f (DV.singleton (DV.head zipped)) (DV.tail zipped)
  where
    f xs (c, v) =
        let (c', v') = DV.last xs 
        in if toDay c == toDay c' then DV.init xs `DV.snoc` (c, comp v v')
                                  else xs         `DV.snoc` (c, v)
    comp = case fi of
               DailyMax -> max
               dailyMin -> min

toDay :: Epoch -> Int
toDay = floor . ( / fromIntegral aday) . fromIntegral

indexedFilter :: (Ord n, V.Storable n) => V.Vector Int -> Filter -> V.Vector n -> V.Vector n
indexedFilter ixs DailyMax xs = V.zipWith (\i j -> V.maximum $ V.slice i j xs) ixs (V.init ixs)
indexedFilter ixs DailyMin xs = V.zipWith (\i j -> V.minimum $ V.slice i j xs) ixs (V.init ixs)

aday :: Int
aday = 60 * 60 * 24
