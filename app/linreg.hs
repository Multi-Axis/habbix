{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Forecast
import Future
import Query (withMaybe)

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Aeson hiding (Result)
import Data.Aeson.TH
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Lazy.Char8 as C
import System.Exit

data Params = Params
            { preFilter :: Maybe Filter }

data Details = Details { r2det :: Double }

$(deriveJSON defaultOptions ''Filter)
$(deriveJSON defaultOptions ''Params)
$(deriveJSON defaultOptions ''Details)

main :: IO ()
main = do
    Just ev <- decode <$> C.getContents
    (res, _) <- runStateT (go ev) $ (,) <$> V.convert . evClocks
                                        <*> V.convert . evValues $ ev
    C.putStrLn (encode res)

go :: Event Params -> Predict (Result Details)
go Event{..} = do
    let Params{..} = evParams
    -- PreFilters
    withMaybe preFilter applyFilter 

    -- lin reg
    params <- uncurry simpleLinearRegression . (V.convert . V.map fromIntegral *** V.convert) <$> get
    case params of
        Nothing -> liftIO $ do
            putStrLn "Ei dataa"
            exitFailure
        Just (a, b, r2) -> do

            let details = Details { r2det = r2 }

            return Result { reClocks  = evDrawFuture
                          , reValues  = drawFuture a b evLast evDrawFuture
                          , reDetails = details }
