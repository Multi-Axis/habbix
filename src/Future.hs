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
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Time
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
import           System.Process (readProcess)

#ifdef STATISTICS
import           Numeric.Statistics
#endif

-- * Events and Results

data Event params = Event
           { evValueType :: Int
           , evClocks :: V.Vector Epoch
           , evValues :: V.Vector Double
           , evDrawFuture :: V.Vector Epoch
           , evParams :: params
           } deriving (Show)

data Result details = Result
            { reClocks :: V.Vector Epoch
            , reValues :: V.Vector Double
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
    forM_ xs $ \dd -> executeModelNextWeek dd >>= replacePredictionInDB dd

-- | Run forecast models against managed item histories.
executeFutures :: Maybe [ItemFutureId] -> Habbix [(Event Object, Result Object)]
executeFutures = getItemFutures >=> mapM executeModelNextWeek

runFuture :: ItemFutureId -> Habbix (Event Object, Result Object)
runFuture futId = head <$> executeFutures (Just [futId])

-- | Update respective tables in the local db.
replacePredictionInDB :: FutureDrawData -> (Event Object, Result Object) -> Habbix ()
replacePredictionInDB (_,_, Value vtype, Value futId, _)  (_, res) = runLocalDB $ replaceFuture futId vtype res

-- * Query history and prediction configuration data

type FutureDrawData =
        (Value ItemId, Value B.ByteString, Value Int, Value ItemFutureId, Value Text)
        -- ^ (items.itemid, item_future.params, items.value_type,
        -- item_future.id, future_model.name)

type Points n = (V.Vector Epoch, DV.Vector n)

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

getDoubleHistory :: ItemFutureId -> DefParams -> Habbix (V.Vector Epoch, DV.Vector Double)
getDoubleHistory i params = do
    Just ItemFuture{..} <- runLocalDB $ get i
    Just Item{..}       <- runLocalDB $ get itemFutureItem
    nowEpoch            <- liftIO getCurrentEpoch

    let query = uncurry (selectHistory' itemFutureItem itemValueType) $ buildQueries nowEpoch params
    runLocalDB $ either (historyVectors >=> return . second (V.convert . DV.map realToFrac))
                        (historyVectors >=> return . second (V.convert . DV.map fromIntegral))
                 query

-- | This looks at pStopLower and pStopUpper on the params and builds
-- queries to filter history (first part of the tuple) or
-- historyUint (snd) columns based on them.
buildQueries :: (Esqueleto m1 expr1 backend1, Esqueleto m expr backend)
             => Epoch -- ^ Today (where pStopLower is relative to when it is < 0)
             -> DefParams
             -> (expr (Entity History) -> m (), expr1 (Entity HistoryUint) -> m1 ())
buildQueries today DefParams{..} =
    (\h -> do
        withMaybe pStopLower $ \l -> where_ $ h ^. HistoryClock >=. val (if l < 0 then today + l else l)
        withMaybe pStopUpper $ \u -> where_ $ h ^. HistoryClock <=. val u
    , \h -> do
        withMaybe pStopLower $ \l -> where_ $ h ^. HistoryUintClock >=. val (if l < 0 then today + l else l)
        withMaybe pStopUpper $ \u -> where_ $ h ^. HistoryUintClock <=. val u
    )

historyVectors :: DPS n -> DB (Points n)
historyVectors src = do
    xs <- src $$ CL.consume
    return (V.fromList (map fst xs), DV.fromList (map snd xs))

-- * Run models

executeModelNextWeek :: FutureDrawData -> Habbix (Event Object, Result Object)
executeModelNextWeek dd = do
    iv <- liftIO nextWeek
    executeModel (const iv) id dd

-- | Execute a single model
executeModel :: (V.Vector Epoch -> V.Vector Epoch) -- ^ Build Event.drawFuture based on the history clocks
             -> (DefParams -> DefParams) -- ^ Modify params relevant inside habbix
             -> FutureDrawData
             -> Habbix (Event Object, Result Object)
executeModel futClocks fParams (Value itemid, Value params, Value vtype, Value _futureid, Value model) = do

    -- $debug
    -- liftIO . putStrLn $ "Running predictions for item " ++ show (fromSqlKey itemid) ++ " and model " ++ unpack model

    case  decodeStrict' params of
        Just p -> do

            nowEpoch <- liftIO getCurrentEpoch

            let (histQuery, uintQuery) = buildQueries nowEpoch (fParams p)
            let query                  = selectHistory' itemid vtype histQuery uintQuery

            (cs, hs) <- runLocalDB $ either (historyVectors >=> return . second (V.convert . DV.map realToFrac))
                                            (historyVectors >=> return . second (V.convert . DV.map fromIntegral))
                                     query

            let ev = Event vtype cs hs (futClocks cs) (fromJust $ decodeStrict' params)

            res <- runModel model ev
            case res of
                Left err   -> error $ "Could not parse the model response: " ++ err
                Right res' -> return (ev, res')

        Nothing -> error $ "item_future.params is BROKEN for id = " ++ show (fromSqlKey itemid)

-- | Execute forecast model named @name@ in @forecast_models/<name>@ with
-- given event and return result.
runModel :: Text -> Event Object -> Habbix (Either String (Result Object))
runModel name ev = do
    let filename = "forecast_models/" ++ unpack name
    res <- liftIO $ readProcess filename [] (unpack . decodeUtf8 . B.concat . BL.toChunks $ encode ev)
    return . eitherDecodeStrict' . encodeUtf8 $ pack res

-- | Replaces the future with given result.
replaceFuture :: Key ItemFuture -> Int -> Result Object -> DB ()
replaceFuture futid 0 Result{..} = do
    delete . from $ \fut -> where_ (fut ^. FutureItem ==. val futid)
    liftIO $ print reDetails
    P.update futid [ItemFutureDetails P.=. B.concat (BL.toChunks (encode reDetails))]
    P.insertMany_ $ zipWith (\c v -> Future futid c (realToFrac v)) (V.toList reClocks) (V.toList reValues)

replaceFuture futid 3 Result{..} = do
    delete . from $ \futuint -> where_ (futuint ^. FutureUintItem ==. val futid)
    P.update futid [ItemFutureDetails P.=. B.concat (BL.toChunks (encode reDetails))]
    P.insertMany_ $ zipWith (\c v -> FutureUint futid c (toRational v)) (V.toList reClocks) (V.toList reValues)

replaceFuture _     n _          = error $ "unknown value_type: " ++ show n

-- * Statistical

futureCompare :: ItemFutureId -> (Epoch, Epoch) -> (Epoch, Epoch) -> Habbix String
#ifdef STATISTICS
futureCompare i (sf, ef) (sr, er) = do
    (x:_)           <- getItemFutures (Just [i])
    (_, Result{..}) <- executeModel (V.takeWhile (<= ef) . V.dropWhile (< sf)) (const $ DefParams Nothing Nothing) x
    (_, realValues) <- getDoubleHistory i $ DefParams (Just sr) (Just er)

    return $ "Spearman: " ++ show (spearman reValues realValues)
#else
futureCompare _ _ _ = error "habbix was not compiled with -fstatistics"
#endif

-- * Utility

withMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
withMaybe m f = maybe (return ()) f m

-- | Epoch timestamps for every day next week
nextWeek :: IO (V.Vector Epoch)
nextWeek = do
    curEpoch <- getCurrentEpoch
    return $ V.fromList [curEpoch, curEpoch + 86400 .. curEpoch + 7 * 86401]

getCurrentEpoch :: IO Epoch
getCurrentEpoch = read . formatTime undefined "%s" <$> getCurrentTime
