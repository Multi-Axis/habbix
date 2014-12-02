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
--
-- The Prediction interface: @Event@s out and @Result@s in.
------------------------------------------------------------------------------
module Future
    ( -- * Forecast interface
    Event(..), Result(..),

    -- * Future and history
    runFuture, executeFutures, executeFutures',
    getItemFutures, historyVectors,
    DP, getDP,

    -- * Models
    runModel, executeModel, executeModelNextWeek

#ifdef STATISTICS
    -- * Statistical
    , futureCompare
#endif
    ) where

import ZabbixDB
import Query

import           Control.Monad
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Aeson hiding (Value, Result)
import           Data.Aeson.TH
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector.Storable as V
import qualified Data.Vector          as DV
import           Database.Esqueleto
import qualified Database.Persist as P
import           Database.Persist.Quasi (PersistSettings(psToDBName), lowerCaseSettings)
import           System.Process (readProcessWithExitCode)
import           System.Exit

#ifdef STATISTICS
import           Numeric.Statistics
#endif

-- * Events and Results

data Event params = Event
           { evValueType :: Int
           , evClocks :: V.Vector Epoch
           , evValues :: V.Vector Double -- ^ (converted from Rational)
           , evLast :: Maybe (Epoch, Double) -- ^ Last value in history (converted from rational)
           , evDrawFuture :: V.Vector Epoch
           , evParams :: params
           } deriving (Show)

data Result details = Result
            { reClocks :: V.Vector Epoch
            , reValues :: V.Vector Double -- ^ (converted to rational)
            , reDetails :: details
            } deriving (Show)

data DefParams = DefParams
               { pStopLower :: Maybe Epoch -- can be negative, then relative to max(time)
               , pStopUpper :: Maybe Epoch
               } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 2 } ''Event)
$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 2 } ''Result)
$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 1 } ''DefParams)

-- * Running predictions

-- | apply @executeFutures@ and replacePredictionInDB
executeFutures' :: Maybe [ItemFutureId] -> Habbix ()
executeFutures' mis = do
    xs <- getItemFutures mis
    forM_ xs $ \dd -> do
        r <- executeModelNextWeek dd
        case r of
            Right r' -> replacePredictionInDB dd r'
            Left er  -> logErrorN $ pack er

-- | Run forecast models against managed item histories.
executeFutures :: Maybe [ItemFutureId] -> Habbix [Either String (Event Object, Result Object)]
executeFutures = getItemFutures >=> mapM executeModelNextWeek

runFuture :: ItemFutureId -> Habbix (Either String (Event Object, Result Object))
runFuture futId = head <$> executeFutures (Just [futId])

-- | Update respective tables in the local db.
replacePredictionInDB :: FutureDrawData -> (Event Object, Result Object) -> Habbix ()
replacePredictionInDB (_,_,_, Value futId, _)  (_, res) =
        runLocalDB $ replaceFuture futId res

-- * Query history and prediction configuration data

type FutureDrawData =
        (Value ItemId, Value B.ByteString, Value Int, Value ItemFutureId, Value Text)
        -- ^ (items.itemid, item_future.params, items.value_type,
        -- item_future.id, future_model.name)

type DP = (V.Vector Epoch, DV.Vector Rational)

getItemFutures :: Maybe [ItemFutureId] -> Habbix [FutureDrawData]
getItemFutures mis = runLocalDB . select . from $ \(item `InnerJoin` itemFut `InnerJoin` futModel) -> do
    on (futModel ^. FutureModelId ==. itemFut ^. ItemFutureModel)
    on (itemFut ^. ItemFutureItem ==. item ^. ItemId)
    withMaybe mis $ \is -> where_ (itemFut ^. ItemFutureId `in_` valList is)
    return ( itemFut  ^. ItemFutureItem
           , itemFut  ^. ItemFutureParams
           , item     ^. ItemValueType
           , itemFut  ^. ItemFutureId
           , futModel ^. FutureModelName )

getDP :: ItemFutureId -> DefParams -> Habbix DP
getDP i DefParams{..} = do
    ItemFuture{..} <- runLocalDB $ getJust i
    Item{..}       <- runLocalDB $ getJust itemFutureItem
    nowEpoch       <- liftIO getCurrentEpoch

    let query = selectHistory' itemFutureItem nowEpoch pStopLower pStopUpper
    runLocalDB $ historyVectors query

historyVectors :: DPS -> DB DP
historyVectors src = do
    xs <- src $$ CL.consume
    return (V.fromList (map fst xs), DV.fromList (map snd xs))

-- * Run models

executeModelNextWeek :: FutureDrawData -> Habbix (Either String (Event Object, Result Object))
executeModelNextWeek dd = do
    iv <- liftIO nextWeek
    executeModel (const iv) id dd

-- | Execute a single model
executeModel :: (V.Vector Epoch -> V.Vector Epoch) -- ^ Build Event.drawFuture based on the history clocks
             -> (DefParams -> DefParams) -- ^ Modify params relevant inside habbix
             -> FutureDrawData
             -> Habbix (Either String (Event Object, Result Object))
executeModel futClocks fParams (Value itemid, Value params, Value vtype, Value futId, Value model) = do

    logInfoN $ "Run future for future item " <> tshow (fromSqlKey futId) <> " (model " <> model <> ")"

    case  eitherDecodeStrict' params of
        Right p -> do
            let DefParams{..} = fParams p

            nowEpoch <- liftIO getCurrentEpoch

            (cs, hs) <- runLocalDB
                $ (historyVectors >=> return . second (V.convert . DV.map fromRational))
                $ selectHistory' itemid nowEpoch pStopLower pStopUpper

            tick <- runLocalDB $ selectHistoryLast itemid
            let ev = Event vtype cs hs (fmap (second fromRational) tick)
                        (futClocks cs) (fromJust $ decodeStrict' params)
            $logDebug (tshow ev)

            r <- runModel model ev
            return $ case r of
                Right r' -> Right (ev, r')
                Left er  -> Left $ "future item " ++ show (fromSqlKey futId) ++ " failed: " ++ er

        Left er -> return $ Left $
            "params could not be parsed: " ++ er ++
            " (item_future.id = " ++ show (fromSqlKey futId) ++ ")"

-- | Execute forecast model named @name@ in @forecast_models/<name>@ with
-- given event and return result.
runModel :: Text -> Event Object -> Habbix (Either String (Result Object))
runModel name ev = do
    dir <- asks modelsDir
    let filename = dir ++ "/" ++ unpack name
    (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode filename [filename] (unpack . decodeUtf8 . B.concat . BL.toChunks $ encode ev)
    return $ case ec of
        ExitSuccess -> eitherDecodeStrict' . encodeUtf8 $ pack stdout
        ExitFailure _ -> Left (stdout ++ "\n\n" ++ stderr)

-- | Replaces the future with given result.
replaceFuture :: Key ItemFuture -> Result Object -> DB ()
replaceFuture futid Result{..} = do
    delete . from $ \fut -> where_ (fut ^. FutureItem ==. val futid)
    P.update futid [ItemFutureDetails P.=. B.concat (BL.toChunks (encode reDetails))]
    P.insertMany_ $ zipWith (\c v -> Future futid c (realToFrac v)) (V.toList reClocks) (V.toList reValues)

-- * Statistical

#ifdef STATISTICS
futureCompare :: ItemFutureId -> (Epoch, Epoch) -> (Epoch, Epoch) -> Habbix String
futureCompare i (sf, ef) (sr, er) = do
    (x:_)           <- getItemFutures (Just [i])
    Right (_, Result{..}) <- executeModel (V.takeWhile (<= ef) . V.dropWhile (< sf)) (const $ DefParams Nothing Nothing) x
    (_, realValues) <- getDP i $ DefParams (Just sr) (Just er)

    return $ "Spearman: " ++ show (spearman reValues $ V.convert $ DV.map fromRational realValues)
#endif

-- * Utility

-- | Epoch timestamps for every day next week
nextWeek :: IO (V.Vector Epoch)
nextWeek = do
    curEpoch <- getCurrentEpoch
    return $ V.fromList [curEpoch, curEpoch + 86400 .. curEpoch + 7 * 86401]
