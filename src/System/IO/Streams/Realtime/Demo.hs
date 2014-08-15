module System.IO.Streams.Realtime.Demo where

import Data.Monoid
import Data.Time
import Data.Time.Clock
import qualified System.IO.Streams as Streams
import System.IO.Streams.Realtime

main :: IO ()
main = do
  inS <- Streams.fromList [1..100::Int]
  t0  <- getCurrentTime
  tS  <- Streams.fromList [addUTCTime x t0 | x <- [1,1.5..10]]
--  let getT = \t0 a -> return $ addUTCTime (fromIntegral (a::Int)) t0
--  inS'  <- runOpts mempty getT inS
  inS' <- atTimes' (jitter 0 <> compress t0 0.5) inS tS
  outS <- Streams.makeOutputStream (print :: Maybe Int -> IO ())
  Streams.connect inS' outS
