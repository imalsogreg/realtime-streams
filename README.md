realtime-streams
================

Manipulate the timing of io-streams

The goal is to offer an API like this

```haskell
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Realtime as RT

instance Streams.Timestamp MyStruct where
  timestamp = eventTime

Streams.connect (RT.replay (RT.jitter 0.5 <> RT.delay 2.0) dataStream) outStream
```

I'm not there yet :)