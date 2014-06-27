{-# LANGUAGE RecordWildCards #-}

module System.IO.Streams.Realtime where

------------------------------------------------------------------------------
import           Control.Concurrent                  (threadDelay)
import           Control.Monad                       (when)
import           Data.Time                           as Time
import           System.Random
------------------------------------------------------------------------------
import           System.IO.Streams                   (InputStream)
import qualified System.IO.Streams                   as Streams
import           System.IO.Streams.Realtime.Internal (TimeOpts(..), runOpts)


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
steady :: Double -> InputStream a -> IO (InputStream a)
steady rate inStream = do
  t0 <- getCurrentTime
  releaseTimes <- Streams.fromList
                  [addUTCTime (realToFrac $ n/rate) t0 | n <- [0..]]
  atTimes inStream releaseTimes


------------------------------------------------------------------------------
returnAt :: a -> UTCTime -> IO a
returnAt a t = do
  tNow <- getCurrentTime
  let dt = diffUTCTime t tNow
  when (dt > 0) $ threadDelay (floor $ dt * 1e6)
  return a
