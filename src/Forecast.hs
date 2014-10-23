------------------------------------------------------------------------------
-- | 
-- Module         : Forecast
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Forecast where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | `simpleLinearRegression xs ys` gives (a, b, r2) for the line
-- y = a * x + b.
simpleLinearRegression :: Fractional n => Vector n -> Vector n
                       -> (n, n, n)
simpleLinearRegression xs ys = (a, b, r2)
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
