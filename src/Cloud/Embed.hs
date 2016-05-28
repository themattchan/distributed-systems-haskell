module Cloud.Embed where
import Control.Distributed.Process
  ( Serializable(..)
  , ProcessId(..)
  , NodeId(..)
  , SendPort(..)
  , ReceivePort(..)
  , Match(..)
  , MonitorAction(..)
  , PeerInfo(..)
  )
import Control.Distributed.Static (RemoteTable, Closure)

--import qualified Control.Distributed.Process.Node as CHN

-- | Lift the Cloud Haskell language into typeclasses, abstracting
-- | over the ProcessM type

class Basic thingy where
  send   :: Serializable a => ProcessId -> a -> thingy ()
  expect :: Serializable a => thingy a

-- Channels
class Chan thingy where
  newChan          :: Serializable a => thingy (SendPort a, ReceivePort a)
  sendChan         :: Serializable a => SendPort a -> a -> thingy ()
  receiveChan      :: Serializable a => ReceivePort a -> thingy a
  mergePortsBiased :: Serializable a => [ReceivePort a] -> thingy (ReceivePort a)
  mergePortsRR     :: Serializable a => [ReceivePort a] -> thingy (ReceivePort a)

-- Messaging
class Msg thingy where
  receiveWait    :: [Match q ()] -> thingy q
  receiveTimeout :: Int -> [Match q ()] -> thingy (Maybe q)
  match          :: Serializable a => (a -> thingy q) -> Match q ()
  matchIf        :: Serializable a => (a -> Bool) -> (a -> thingy q) -> thingy q ()
  matchUnknown   :: thingy q -> Match q ()

-- Process management
class ProcMan thingy where
  spawn       :: NodeId -> Closure (thingy ()) -> thingy ProcessId
  call        :: Serializable a => NodeId -> Closure (thingy a) -> thingy a
  terminate   :: thingy a
  getSelfPid  :: thingy ProcessId
  getSelfNode :: thingy NodeId

-- Process monitoring
class ProcMon thingy where
  linkProcess    :: ProcessId -> thingy ()
  monitorProcess :: ProcessId -> ProcessId -> MonitorAction -> thingy ()

-- Initialization
class Init thingy io where
  -- type RemoteTable = [(String,Dynamic)]
  runRemote :: Maybe FilePath -> [RemoteTable] -> (String -> thingy ()) -> io ()
  -- type PeerInfo = Map String [NodeId]
  getPeers :: thingy PeerInfo
  findPeerByRole :: PeerInfo -> String -> [NodeId]

-- Logging
class Log thingy where
  say :: String -> thingy ()
