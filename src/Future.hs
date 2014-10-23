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
import           Data.Char (toLower)
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

import           Numeric.Statistics

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

type Points n = (V.Vector Epoch, DV.Vector n)

$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 2 } ''Event)
$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 2 } ''Result)
$(deriveJSON defaultOptions{fieldLabelModifier = unpack . psToDBName lowerCaseSettings . pack . drop 1 } ''DefParams)

-- | Run forecast models against managed item histories and update
-- respective tables in local db.
executeFutures :: Maybe [ItemFutureId] -> Habbix ()
executeFutures mis = do
    iv    <- liftIO nextWeek
    stuff <- getItemFutures mis
    forM_ stuff $ \x@(_, _, Value vtype, Value futureId, _) ->
        executeModel (const iv) id x >>= runLocalDB . replaceFuture futureId vtype . snd

-- getItemFutures :: Maybe
getItemFutures mis = runLocalDB . select . from $ \(item `InnerJoin` itemFut `InnerJoin` futModel) -> do
    on (futModel ^. FutureModelId ==. itemFut ^. ItemFutureModel)
    on (itemFut ^. ItemFutureItem ==. item ^. ItemId)
    withMaybe mis $ \is -> where_ (itemFut ^. ItemFutureId `in_` valList is)
    return ( itemFut  ^. ItemFutureItem
           , itemFut  ^. ItemFutureParams
           , item     ^. ItemValueType
           , itemFut  ^. ItemFutureId
           , futModel ^. FutureModelName )

-- | Execute a single model
executeModel :: (V.Vector Epoch -> V.Vector Epoch) -> (DefParams -> DefParams)
             -> (Value ItemId, Value B.ByteString, Value Int, Value ItemFutureId, Value Text)
             -> Habbix (Event Object, Result Object)
executeModel futClocks fParams (Value itemid, Value params, Value vtype, Value futureid, Value model) = do
    liftIO . putStrLn $ "Running predictions for item "
        ++ show (fromSqlKey itemid) ++ " and model " ++ unpack model
    case  decodeStrict' params of
        Just p -> do
            let (histQuery, uintQuery) = buildQueries $ fParams p
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

nextWeek :: IO (V.Vector Epoch)
nextWeek = do
    curEpoch <- read . formatTime undefined "%s" <$> getCurrentTime
    return $ V.fromList [curEpoch, curEpoch + 86400 .. curEpoch + 7 * 86401]

getDoubleHistory i params = do
    Just ItemFuture{..} <- runLocalDB $ get i
    Just Item{..}       <- runLocalDB $ get itemFutureItem

    let query = uncurry (selectHistory' itemFutureItem itemValueType) $ buildQueries params
    runLocalDB $ either (historyVectors >=> return . second (V.convert . DV.map realToFrac))
                        (historyVectors >=> return . second (V.convert . DV.map fromIntegral))
                 query

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
    return (V.fromList (map fst xs), DV.fromList (map snd xs))

-- | Execute forecast model named @name@ in @forecast_models/<name>@ with
-- given event and return result.
runModel :: Text -> Event Object -> Habbix (Either String (Result Object))
runModel name ev = do
    let filename = "forecast_models/" ++ unpack name
    res <- liftIO $ readProcess filename [] (unpack $ decodeUtf8 $ B.concat . BL.toChunks $ encode ev)
    return . eitherDecodeStrict' . encodeUtf8 $ pack res

-- | replaces the future with given result.
replaceFuture :: Key ItemFuture -> Int -> Result Object -> DB ()
replaceFuture futid 0 Result{..} = do
    delete . from $ \fut -> where_ (fut ^. FutureItem ==. val futid)
    P.insertMany_ $ zipWith (\c v -> Future futid c (realToFrac v)) (V.toList reClocks) (V.toList reValues)
replaceFuture futid 3 Result{..} = do
    delete . from $ \futuint -> where_ (futuint ^. FutureUintItem ==. val futid)
    P.insertMany_ $ zipWith (\c v -> FutureUint futid c (toRational v)) (V.toList reClocks) (V.toList reValues)
replaceFuture _     n _          = error $ "unknown value_type: " ++ show n

futureCompare :: ItemFutureId -> (Epoch, Epoch) -> (Epoch, Epoch) -> Habbix ()
futureCompare i (sf, ef) (sr, er) = do
    (x:_)           <- getItemFutures (Just [i])
    (_, Result{..}) <- executeModel (V.takeWhile (<= ef) . V.dropWhile (< sf)) (const $ DefParams Nothing Nothing) x
    (_, realValues) <- getDoubleHistory i $ DefParams (Just sr) (Just er)

    liftIO $ putStrLn $ "Spearman: " ++ show (spearman reValues realValues)
