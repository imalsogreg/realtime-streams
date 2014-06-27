module Main where

------------------------------------------------------------------------------
import           Test.Framework
------------------------------------------------------------------------------
import qualified System.IO.Streams.Realtime.Tests
import qualified System.IO.Streams.Realtime.Internal.Tests


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain [tests]


------------------------------------------------------------------------------
tests :: Test
tests = mutuallyExclusive $ testGroup "realtime-streams"
        [ System.IO.Streams.Realtime.Tests.tests
        , System.IO.Streams.Realtime.Internal.Tests.tests
        ]
  
