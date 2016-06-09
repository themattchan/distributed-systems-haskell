module Cloud.Native where
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CHN

import Cloud.Embed
-- | Map the lifted language back to their implementations

instance Basic CH.Process where
  send    = CH.send
  expect  = CH.expect

instance Chan CH.Process where
  newChan          = CH.newChan
  sendChan         = CH.sendChan
  receiveChan      = CH.receiveChan
  mergePortsBiased = CH.mergePortsBiased
  mergePortsRR     = CH.mergePortsRR

instance Msg CH.Process where
  receiveWait    = CH.receiveWait
  receiveTimeout = CH.receiveTimeout
  match          = CH.match
  matchIf        = CH.matchIf
  matchUnknown   = CH.matchUnknown

-- Process management
instance ProcMan CH.Process where
  spawn          = CH.spawn
  call           = CH.call
  terminate      = CH.terminate
  getSelfPid     = CH.getSelfPid
  getSelfNode    = CH.getSelfNode

-- Process monitoring
-- instance ProcMon CH.Process where
--   linkProcess     = CH.linkProcess
-- --  monitorProcess  = CH.monitorProcess

-- -- Initialization
-- instance Init CH.Process where
--   runRemote      = CH.runRemote
-- --  getPeers       = CH.getPeers
-- --  findPeerByRole = CH.findPeerByRole

-- Logging
instance Log CH.Process where
  say = CH.say
