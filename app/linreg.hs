{-# LANGUAGE RecordWildCards #-}
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

import Forecast
import Future

import Control.Applicative
import Data.Aeson
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
    Just Event{..} <- decode <$> C.getContents

    let (a, b) = simpleLinearRegression (V.map fromIntegral evClocks) evValues
        aday   = 60 * 60 * 24
        clocks = V.iterateN 7 (+ aday) (V.last evClocks)
        values = V.map (\x -> a * fromIntegral x + b) clocks
        res    = Result clocks values mempty

    C.putStrLn (encode res)
