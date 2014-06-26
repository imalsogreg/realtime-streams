realtime-streams
================

Manipulate the timing of io-streams

The goal is to offer an API like this for controlling the timing of io-streams data.

```haskell
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Realtime as RT

...

instance RT.Timestamp MyStruct where
  timestamp = myEventTime

S.connect (RT.replay (RT.jitter 0.5 <> RT.delay 2.0) myInStream) someOutStream
```

I'm not there yet :)