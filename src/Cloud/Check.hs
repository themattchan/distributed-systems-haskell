module Cloud.Check where
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CHN

-- | Implement the language wrt another type, that hopefully obeys the original
-- semantics of the combinators in the Cloud Haskell library

data CheckProc = undefined

instance Basic ProcessM where
  send    = CH.send
  expect  = CH.expect

instance Chan ProcessM where
  newChan          = CH.newChan
  sendChan         = CH.sendChan
  receiveChan      = CH.receiveChan
  mergePortsBiased = CH.mergePortsBiased
  mergePortsRR     = CH.mergePortsRR

instance Msg ProcessM where
  receiveWait    = CH.receiveWait
  receiveTimeout = CH.receiveTimeout
  match          = CH.match
  matchIf        = CH.matchIf
  matchUnknown   = CH.matchUnknown

-- Process management
class ProcMan ProcessM where
  spawn          = CH.spawn
  call           = CH.call
  terminate      = CH.terminate
  getSelfPid     = CH.getSelfPid
  getSelfNode    = CH.getSelfNode

-- Process monitoring
class ProcMon ProcessM where
  linkProcess     = CH.linkProcess
  monitorProcess  = CH.monitorProcess

-- Initialization
class Init ProcessM where
  runRemote      = CH.runRemote
  getPeers       = CH.getPeers
  findPeerByRole = CH.findPeerByRole

-- Logging
class Log ProcessM where
  say = CH.say
