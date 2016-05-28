module Cloud.Check where
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CHN

-- | Implement the language wrt another type, that hopefully obeys the original
-- semantics of the combinators in the Cloud Haskell library

data ProcessC = ProcessC {

instance Basic Process where
  send    = CH.send
  expect  = CH.expect

instance Chan Process where
  newChan          = CH.newChan
  sendChan         = CH.sendChan
  receiveChan      = CH.receiveChan
  mergePortsBiased = CH.mergePortsBiased
  mergePortsRR     = CH.mergePortsRR

instance Msg Process where
  receiveWait    = CH.receiveWait
  receiveTimeout = CH.receiveTimeout
  match          = CH.match
  matchIf        = CH.matchIf
  matchUnknown   = CH.matchUnknown

-- Process management
instance ProcMan Process where
  spawn          = CH.spawn
  call           = CH.call
  terminate      = CH.terminate
  getSelfPid     = CH.getSelfPid
  getSelfNode    = CH.getSelfNode

-- Process monitoring
instance ProcMon Process where
  linkProcess     = CH.linkProcess
  monitorProcess  = CH.monitorProcess

-- Initialization
instance Init Process where
  runRemote      = CH.runRemote
  getPeers       = CH.getPeers
  findPeerByRole = CH.findPeerByRole

-- Logging
instance Log Process where
  say = CH.say
