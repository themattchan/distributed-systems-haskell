module Misc where
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CHN

-- runProcess :: LocalNode -> Process () -> IO ()

-- https://hackage.haskell.org/package/distributed-process-0.6.1/docs/Control-Distributed-Process.html
class MyCH a where
  send :: CH.ProcessId -> b -> a
  expect :: a


-- Cloud Haskell impls, returns Process.
instance MyCH (CH.Process a) where
  send = CH.send
  expect = CH.expect
