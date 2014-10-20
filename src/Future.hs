{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Future
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Future where

import ZabbixDB
import Query

import           Control.Monad
import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Aeson hiding (Result)
import           Data.Aeson.TH
import           Data.ByteString.Lazy (toStrict)
import           Data.Char (toLower)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector as V
import           Database.Esqueleto
import qualified Database.Persist as P
import           System.Process (readProcess)

data Event params = Event
           { evValueType :: Int
           , evClocks :: V.Vector Epoch
           , evValues :: V.Vector Double
           , evParams :: params
           }

data Result details = Result
            { reClocks :: V.Vector Epoch
            , reValues :: V.Vector Double
            , reDetails :: details
            }

data DefParams = DefParams
               { pStopLower :: Maybe Epoch -- can be negative, then relative to max(time)
               , pStopUpper :: Maybe Epoch
               }

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 2 } ''Event)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 2 } ''Result)
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 1 } ''DefParams)

type Points n = (V.Vector Epoch, V.Vector n)

-- | Run forecast models against managed item histories and update
-- respective tables in local db.
executeFutures :: Habbix ()
executeFutures = do
    itemIds <- runLocalDB . select . from $ \(item `InnerJoin` itemFut `InnerJoin` futModel) -> do
        on (futModel ^. FutureModelId ==. itemFut ^. ItemFutureModel)
        on (itemFut ^. ItemFutureItem ==. item ^. ItemId)
        return ( itemFut  ^. ItemFutureItem
               , itemFut  ^. ItemFutureParams
               , item     ^. ItemValueType
               , itemFut  ^. ItemFutureId
               , futModel ^. FutureModelName
               )

    forM_ itemIds $ \(Value itemid, Value params, Value vtype, Value futureid, Value model) -> do

        liftIO . putStrLn $ "Running predictions for item "
            ++ show (fromSqlKey itemid) ++ " and model " ++ unpack model

        let (histQuery, uintQuery) = buildQueries (fromJust $ decodeStrict' params)
            query                  = selectHistory' itemid vtype histQuery uintQuery

        (cs, hs) <- runLocalDB $ either (historyVectors >=> return . second (V.map realToFrac))
                                        (historyVectors >=> return . second (V.map fromIntegral))
                                 query

        let ev = Event vtype cs hs (fromJust $ decodeStrict' params)

        res <- runModel model ev
        case res of
            Left err   -> error $ "Could not parse the model response: " ++ err
            Right res' -> runLocalDB $ replaceFuture futureid vtype res'

-- buildQueries :: DefParams -> (a -> sqlquery, b -> sqlquery)
buildQueries DefParams{..} = 
    (\h -> do
        withMaybe pStopLower $ \l -> where_ $ h ^. HistoryClock >=. val l
        withMaybe pStopUpper $ \u -> where_ $ h ^. HistoryClock <=. val u
    , \h -> do
        withMaybe pStopLower $ \l -> where_ $ h ^. HistoryUintClock >=. val l
        withMaybe pStopUpper $ \u -> where_ $ h ^. HistoryUintClock <=. val u
    )

withMaybe m f = maybe (return ()) f m

historyVectors :: DPS n -> DB (Points n)
historyVectors src = do
    xs <- src $$ CL.consume
    return (V.fromList (map fst xs), V.fromList (map snd xs))

-- | Execute forecast model named @name@ in @forecast_models/<name>@ with
-- given event and return result.
runModel :: Text -> Event Object -> Habbix (Either String (Result Object))
runModel name ev = do
    let filename = "forecast_models/" ++ unpack name
    res <- liftIO $ readProcess filename [] (unpack $ decodeUtf8 $ toStrict $ encode ev)
    return . eitherDecodeStrict' . encodeUtf8 $ pack res

-- | replaces the future with given result.
replaceFuture :: Key ItemFuture -> Int -> Result Object -> DB ()
replaceFuture futid 0 Result{..} = do
    delete . from $ \fut -> where_ (fut ^. FutureItem ==. val futid)
    P.insertMany_ $ V.toList $ V.zipWith (\c v -> Future futid c (realToFrac v)) reClocks reValues
replaceFuture futid 3 Result{..} = do
    delete . from $ \futuint -> where_ (futuint ^. FutureUintItem ==. val futid)
    P.insertMany_ $ V.toList $ V.zipWith (\c v -> FutureUint futid c (toRational v)) reClocks reValues
replaceFuture _     n _          = error $ "unknown value_type: " ++ show n
