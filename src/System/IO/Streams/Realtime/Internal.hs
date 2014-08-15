{-# LANGUAGE RecordWildCards #-}

module System.IO.Streams.Realtime.Internal where

------------------------------------------------------------------------------
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM   (atomically, modifyTVar, newTVar,
                                           readTVar, retry, writeTVar)
import           Control.Monad            ((<=<), (>=>), when)
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
  mempty = TimeOpts 1 return
  TimeOpts r m `mappend` TimeOpts r' m' =
    TimeOpts (max r r') (m >=> m')


------------------------------------------------------------------------------
runOpts :: TimeOpts
        -> (UTCTime -> a -> IO UTCTime)
        -> InputStream a
        -> IO (InputStream a)
runOpts TimeOpts {..} timeOf inS' = do
  inS <- Streams.lockingInputStream inS'
  buffer <- atomically $ newTVar Map.empty
  t0 <- getCurrentTime
  _ <- forkIO $ feedBuffer buffer inS t0
  drawFromBuffer buffer
  
  where

    feedBuffer buffer inS t0 = go
      where
        go = do
          a  <- Streams.read inS
          maybe (return $! ()) (\v -> feedOne buffer t0 v >> go) a

    feedOne buffer t0 a = do
      t  <- getCurrentTime
      aT <- modifyTime <=< (timeOf t0) $ a
      let dt = diffUTCTime aT (addUTCTime readAhead t)
      when (dt > 0) (threadDelay $ floor (dt * 1e6))
      atomically $ modifyTVar buffer (Map.insert aT a)

    drawFromBuffer buffer = Streams.makeInputStream (runRace buffer)

    runRace buffer = do
      (k,a) <- getBufferHead buffer
      t <- getCurrentTime 
      let dt = diffUTCTime k t
      res <- race
             (threadDelay $ floor (dt * 1e6))
             (getNewerBufferHead k buffer)
      case res of
        Left  () -> print "Left" >> (return $ Just a)
        Right () -> do
          print "Collission"
          runRace buffer

    getBufferHead buffer = atomically $ do
      b <- readTVar buffer
      case Map.minViewWithKey b of
        Nothing -> retry
        (Just ((k,a), theRest)) -> do
          writeTVar buffer theRest
          return (k,a)

    getNewerBufferHead k buffer = atomically $ do
      b <- readTVar buffer
      case Map.minViewWithKey b of
        Nothing -> retry
        (Just ((k',_), _)) ->
          when (k' > k) retry >> 
          return ()
