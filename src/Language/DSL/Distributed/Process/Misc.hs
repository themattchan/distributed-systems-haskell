module Misc where
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CHN

-- runProcess :: LocalNode -> Process () -> IO ()

class MyCH a where
  send ::
  expect ::


instance MyCH (CH.Process ()) where
