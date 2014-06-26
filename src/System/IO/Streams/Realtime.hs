{-# LANGUAGE RecordWildCards #-}

module System.IO.Streams.Realtime where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Ord
import Data.Time
import Data.Time.Clock
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Realtime.Internal
import System.Random

------------------------------------------------------------------------------
atTimes :: InputStream a -> InputStream UTCTime -> IO (InputStream a)
atTimes = Streams.zipWithM returnAt


------------------------------------------------------------------------------
atTimes' :: TimeOpts -> InputStream a -> InputStream UTCTime -> IO (InputStream a)
atTimes' opt inS tS = do
  stampedStream <- Streams.zip inS tS
  runOpts opt (\t0 a -> return $ snd a) stampedStream >>= Streams.map fst


------------------------------------------------------------------------------
jitter :: Double -> TimeOpts  -- TODO Fix this up
jitter stDev = TimeOpts (realToFrac (5 * stDev))
               (\t -> do dt <- randomRIO (-2*stDev,2*stDev)
                         return $ addUTCTime (realToFrac dt) t
               )

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

------------------------------------------------------------------------------
steady :: Double -> InputStream a -> IO (InputStream a)
steady rate inStream = do
  t0 <- getCurrentTime
  releaseTimes <- Streams.fromList [addUTCTime (realToFrac $ n/rate) t0
                                   | n <- [0..]]
  atTimes inStream releaseTimes


------------------------------------------------------------------------------
returnAt :: a -> UTCTime -> IO a
returnAt a t = do
  tNow <- getCurrentTime
  let dt = diffUTCTime t tNow
  when (dt > 0) $ threadDelay (floor $ dt * 1e6)
  return a
