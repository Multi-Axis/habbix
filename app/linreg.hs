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
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as C

type Ev = Event Params

data Params = Params
            { stopLower :: Maybe Epoch
            , stopUpper :: Maybe Epoch
            }

data Details = Details
             { determination :: Double }

$(deriveJSON defaultOptions ''Params)
$(deriveJSON defaultOptions ''Details)

main :: IO ()
main = do
    Just Event{..} <- decode <$> C.getContents

    let xs     = stopLower evParams
        (a, b) = simpleLinearRegression (fromIntegral <$> evClocks) evValues
        clocks = V.iterateN 7 (+ aday) (V.last evClocks)
        values = (\x -> a * fromIntegral x + b) <$> clocks
        res    = Result clocks values $ Details 0 -- TODO details

    C.putStrLn (encode res)
  where
    aday = 60 * 60 * 24
