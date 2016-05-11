> module Misc where
> import qualified Control.Distributed.Process as CH
> import qualified Control.Distributed.Process.Node as CHN

-- runProcess :: LocalNode -> Process () -> IO ()

-- https://hackage.haskell.org/package/distributed-process-0.6.1/docs/Control-Distributed-Process.html
-- the DSL takes a `Process` thing and


newtype Process a = Process {
    unProcess :: ReaderT LocalProcess IO a
      }
        deriving ( Applicative
		 , Functor
		 , Monad
		 , MonadFix
		 , MonadIO
		 , MonadReader LocalProcess
		 , Typeable
		 )
-}

> class MyCH a where
>   send :: CH.ProcessId -> b -> a
>   expect :: a
> 
> 
> -- Cloud Haskell impls, returns Process.
> instance MyCH (CH.Process a) where
>   send = CH.send
>   expect = CH.expect


The problem:  TTF is slightly different than the cloudhaskell dsl because the combinators for a dsl are in the dsl itself, but for Process it's the monad typeclass.

Can think of the various derived typeclasses of Process as the shallow embedding itself... override default defns? Cannot override IO



Paper: http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf
Look @ figure 2

Example

send :: Serializable a => ProcessId -> a -> ProcessM ()
expect :: Serializable a => ProcessM a

data Ping = Ping ProcessId
data Pong = Pong ProcessId
−− omitted: Serializable instance for Ping and Pong

ping :: ProcessM ()
ping = do { Pong partner <-expect
          ; self <-getSelfPid
          ; send partner (Ping self )
          ; ping }

Defining the DSL

- is ProcessM the object language, and the combinators on ProcessM the meta bits?
- is ProcessM a datatype of the object language?

If you view Cloud Haskell as a shallow embedding, then ProcessM is a basic type the embedding evaluates to, and the combinators are the language constructs.
This means that all the combinators should be hoisted into the new embedding, so you can think of an existing Cloudhaskell program as being automatically parametrised by this new class.
Problem: they are not closed...


instance Monad ProcessM
instance MonadIO ProcessM
send :: Serializable a => ProcessId -> a -> ProcessM ()
expect :: Serializable a => ProcessM a
 -- Channels
newChan :: Serializable a => ProcessM (SendPort a, ReceivePort a)
sendChan :: Serializable a => SendPort a -> a -> ProcessM ()
receiveChan :: Serializable a => ReceivePort a -> ProcessM a
mergePortsBiased :: Serializable a => [ReceivePort a] -> ProcessM (ReceivePort a)
mergePortsRR :: Serializable a => [ReceivePort a] -> ProcessM (ReceivePort a)
-- Advanced messaging
instance Monad MatchM
  receiveWait :: [MatchM q ()] -> ProcessM q
  receiveTimeout :: Int -> [MatchM q ()] -> ProcessM (Maybe q)
  match :: Serializable a => (a -> ProcessM q) ->MatchM q ()
  matchIf :: Serializable a => (a -> Bool) -> (a -> ProcessM q) ->MatchM q ()
  matchUnknown:: ProcessM q ->MatchM q ()

-- Process management
spawn :: NodeId -> Closure (ProcessM ()) -> ProcessM ProcessId
call :: Serializable a => NodeId -> Closure (ProcessM a) -> ProcessM a
terminate :: ProcessM a
getSelfPid :: ProcessM ProcessId
getSelfNode :: ProcessM NodeId

-- Process monitoring
linkProcess :: ProcessId -> ProcessM ()
monitorProcess :: ProcessId -> ProcessId -> MonitorAction -> ProcessM ()

-- Initialization
type RemoteTable = [(String,Dynamic)]
runRemote :: Maybe FilePath -> [RemoteTable] -> ( String -> ProcessM ()) -> IO ()
type PeerInfo = Map String [NodeId]
getPeers :: ProcessM PeerInfo
findPeerByRole :: PeerInfo -> String -> [NodeId]

--Syntactic sugar
mkClosure :: Name -> Q Exp
remotable :: [Name] ->Q [Dec]

-- Logging
say :: String -> ProcessM ()

-- Type classes
class (Binary a,Typeable a) => Serializable a
class Typeable a where typeOf :: a -> TypeRep
class Binary t where {put :: t -> PutM (); get :: Get t}
encode :: Binary a => a -> ByteString
−− Defined in terms of put
decode :: Binary a => ByteString -> a
−− Defined in terms of get



