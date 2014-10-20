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
            { preFilter :: Maybe Bool
            }

data Details = Details
             { r2det :: Double }

$(deriveJSON defaultOptions ''Params)
$(deriveJSON defaultOptions ''Details)

main :: IO ()
main = do
    Just Event{..} <- decode <$> C.getContents
    let Params{..} = evParams

    let (a, b, r2) = simpleLinearRegression (fromIntegral <$> evClocks) evValues

    let details = Details { r2det = r2 }
        res     = Result  { reClocks  = V.iterateN 7 (+ aday) (V.last evClocks)
                          , reValues  = (\x -> a * fromIntegral x + b) <$> reClocks res
                          , reDetails = details
                          }
    C.putStrLn (encode res)
  where
    aday = 60 * 60 * 24
