{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Streams.Realtime.Internal where

------------------------------------------------------------------------------
import           Control.Concurrent       (threadDelay)
--import           Control.Concurrent.Async (race)
--import           Control.Concurrent.STM   (atomically, modifyTVar, newTVar,
--                                           readTVar, retry, writeTVar)
import           Control.Monad            ((<=<), (>=>), when)
import           Data.IORef
import qualified Data.Map                 as Map
import           Data.Monoid              (Monoid, mempty, mappend)
import           Data.Time                (UTCTime)
import           Data.Time.Clock          (getCurrentTime, addUTCTime,
                                           diffUTCTime, NominalDiffTime)
------------------------------------------------------------------------------
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams


------------------------------------------------------------------------------
data TimeOpts = TimeOpts {
    readAhead  :: NominalDiffTime
  , modifyTime :: UTCTime -> IO UTCTime
  }


instance Monoid TimeOpts where
  mempty = TimeOpts 0 return
  TimeOpts r m `mappend` TimeOpts r' m' =
    TimeOpts (max r r') (m <=< m')


------------------------------------------------------------------------------
runOpts :: forall a. TimeOpts
        -> (UTCTime -> a -> IO UTCTime)
        -> InputStream a
        -> IO (InputStream a)
runOpts TimeOpts {..} timeOf inS = do
  t0     <- getCurrentTime
  mapRef <- newIORef Map.empty
  Streams.makeInputStream $ go mapRef t0
  where
    go :: IORef (Map.Map UTCTime a) -> UTCTime -> IO (Maybe a)
    go mR t0 = do
      m     <- readIORef mR
      m'    <- pull m t0
      tNow' <- getCurrentTime
      (m'', x) <- push m' tNow'
      writeIORef mR m''
      return x

    needToPull :: Map.Map UTCTime a -> IO Bool
    needToPull m = getCurrentTime >>= \tNow -> do
      return $ (||) (Map.null m) $
        (addUTCTime readAhead tNow) > fst (Map.findMax m)

    pull :: Map.Map UTCTime a -> UTCTime -> IO (Map.Map UTCTime a)
    pull m t0 = Streams.read inS >>= \x' -> case x' of
      Nothing -> return m
      Just x  -> do
        xT <- (timeOf t0 >=> modifyTime) x :: IO UTCTime
        let m' = Map.insert xT x m
        needPull <- needToPull m'
        case needPull of
          False -> return m'
          True  -> pull m' t0

    push :: Map.Map UTCTime a -> UTCTime -> IO (Map.Map UTCTime a, Maybe a)
    push m tNow
      | Map.null m = return (m,Nothing)
      | otherwise  = do
        let (xT,x) = Map.findMin m
            dt    = diffUTCTime xT tNow
        threadDelay . floor . (*1e6) $ dt
        return (Map.deleteMin m, Just x)
