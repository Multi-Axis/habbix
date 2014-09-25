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

import qualified Data.Vector as V
import System.IO
import Forecast

main :: IO ()
main = do
    input <- getContents
    let (xs, ys) = unzip $ map read $ lines input :: ([Double], [Double])
        (a, b)   = simpleLinearRegression (V.fromList xs) (V.fromList ys)
    print (a, b)
