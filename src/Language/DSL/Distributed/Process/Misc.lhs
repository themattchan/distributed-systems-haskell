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



What about a series of typeclasses, each implementing a part of the language? How to combine?

class Basic thingy where
  -- send :: Serializable a => ProcessId -> a
  --      -> ProcessM ()
  -- expect :: Serializable a
  --        => ProcessM a

  send :: Serializable a => ProcessId -> a
       -> thingy ()
  expect :: Serializable a
         => thingy a


class Chan thingy where
  newChan :: Serializable a => thingy (SendPort a, ReceivePort a)
  sendChan :: Serializable a => SendPort a -> a -> thingy ()
  receiveChan :: Serializable a => ReceivePort a -> thingy a
  mergePortsBiased :: Serializable a => [ReceivePort a] -> thingy (ReceivePort a)
  mergePortsRR :: Serializable a => [ReceivePort a] -> thingy (ReceivePort a)

class Msg stx where
  receiveWait ::



-----------------------

-- instances for ProcessM are just the library definitions
instance Basic ProcessM where
  send = CH.send
  expect = CH.expect

instance Chan ProcessM where
  newChan = CH.newChan
  sendChan = CH.sendChan
  receiveChan = CH.receiveChan
  mergePortsBiased = CH.mergePortsBiased
  mergePortsRR = CH.mergePortsRR

-----------------------

Should be able to do this:

ping :: (Basic ProcessM, Chan ProcessM, Msg ProcessM ...) => ProcessM ()
ping = do { Pong partner <-expect
          ; self <-getSelfPid
          ; send partner (Ping self )
          ; ping }


ping :: (Basic ProcessCheck, Chan ProcessCheck, Msg ProcessCheck ...) => ProcessCheck ()
ping = do { Pong partner <-expect
          ; self <-getSelfPid
          ; send partner (Ping self )
          ; ping }

etc etc

Questions:
1. Is this what we want?
2. Semantics are implemented in the instances, isn't this project all about running the "same semantics" for the combinators over different result types?
3. How to reuse the definitions?
4. Is the parametrisation recycling too much of the library code?