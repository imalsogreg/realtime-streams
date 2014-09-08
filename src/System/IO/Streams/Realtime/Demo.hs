module System.IO.Streams.Realtime.Demo where

import Data.Monoid
import Data.Time
import Data.Time.Clock
import qualified System.IO.Streams as Streams
import System.IO.Streams.Realtime

demo :: IO ()
demo = do
  inS <- Streams.fromList ['A'..'z'::Char]
  t0  <- getCurrentTime
  tS  <- Streams.fromList [addUTCTime x t0 | x <- [1,2..10]]
  inS' <- atTimes' (jitter 0 <> compress t0 100 <> jitter 0.1) inS tS
  outS <- Streams.makeOutputStream (print :: Maybe Char -> IO ())
  Streams.connect inS' outS
