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

data Event = Event
           { evValueType :: Int
           , evClocks :: V.Vector Epoch
           , evValues :: V.Vector Double
           , evParams :: Object
           }
data Result = Result
            { reClocks :: V.Vector Epoch
            , reValues :: V.Vector Double
            , reDetails :: Object
            }

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4 } ''Event)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4 } ''Result)

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

        liftIO $ putStrLn $ "Running predictions for item " ++ show (fromSqlKey itemid) ++ " and model " ++ unpack model

        (cs, hs) <- runLocalDB $ either (historyVectors >=> return . second (V.map realToFrac))
                                        (historyVectors >=> return . second (V.map fromIntegral))
                               $ selectHistory itemid vtype
        let ev = Event vtype cs hs (fromJust $ decodeStrict' params)
        runModel model ev
            >>= maybe (error "Model returned Nothing") (runLocalDB . replaceFuture futureid vtype)

historyVectors :: DPS n -> DB (Points n)
historyVectors src = do
    xs <- src $$ CL.consume
    return (V.fromList (map fst xs), V.fromList (map snd xs))

-- | Execute forecast model named @name@ in @forecast_models/<name>@ with
-- given event and return result.
runModel :: Text -> Event -> Habbix (Maybe Result)
runModel name ev = do
    let filename = "forecast_models/" ++ unpack name
    res <- liftIO $ readProcess filename [] (unpack $ decodeUtf8 $ toStrict $ encode ev)
    return . decodeStrict' . encodeUtf8 $ pack res

-- | replaces the future with given result.
replaceFuture :: Key ItemFuture -> Int -> Result -> DB ()
replaceFuture futid 0 Result{..} = do
    delete . from $ \fut -> where_ (fut ^. FutureItem ==. val futid)
    P.insertMany_ $ V.toList $ V.zipWith (\c v -> Future futid c (realToFrac v)) reClocks reValues

replaceFuture futid 3 Result{..} = do
    delete . from $ \futuint -> where_ (futuint ^. FutureUintItem ==. val futid)
    P.insertMany_ $ V.toList $ V.zipWith (\c v -> FutureUint futid c (toRational v)) reClocks reValues

replaceFuture _ _ _ = error "unknown value_type"
