module System.IO.Streams.Realtime.Internal where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Clock

data TimeOpts = TimeOpts {
    readAhead  :: NominalDiffTime
  , modifyTime :: UTCTime -> IO UTCTime
  }

instance Monoid TimeOpts where
  mempty = TimeOpts 1 return
  TimeOpts r m `mappend` TimeOpts r' m' =
    TimeOpts (max r r') (m >=> m') -- (\t -> do {t' <- m t; m' t'})

{-
data TimeState a = TimeState {
    buffer     :: MVar (Map.Map UTCTime a)
  , tStart     :: UTCTime
}
-}
