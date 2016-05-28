\documentclass{article}
%include polycode.fmt
\usepackage{filecontents}

\begin{filecontents}{\jobname.bib}

@inproceedings{towards,
  title={Towards Haskell in the cloud},
  author={Epstein, J. and Black, A.P. and Peyton-Jones, S.},
  booktitle={ACM SIGPLAN Notices},
  volume={46},
  number={12},
  pages={118--129},
  year={2011},
  organization={ACM},
  ee={http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf}
}

@misc{cloudhs,
  title = {Cloud Haskell 0.6.1}
  howpublished = {https://hackage.haskell.org/package/distributed-process-0.6.1/docs/Control-Distributed-Process.html}
}

\end{filecontents}

\usepackage{natbib}
\bibliographystyle{plainnat}
\bibliography{\jobname}


\long\def\ignore#1{}

\title{Retrofitting to Typed Tagless Final Embeddings for Program Testing}

\begin{document}
\maketitle

\ignore{
> module Cloud.TTF where
> import qualified Control.Distributed.Process as CH
> import qualified Control.Distributed.Process.Node as CHN
}

The Cloud Haskell library\cite{cloudhs} described in\cite{towards}

-- runProcess :: LocalNode -> Process () -> IO ()

--
-- the DSL takes a `Process` thing and

\begin{code}
newtype Process a = Process { unProcess :: ReaderT LocalProcess IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadFix
           , MonadIO
           , MonadReader LocalProcess
           , Typeable
           )
\end{code}

The problem:  TTF is slightly different than the cloudhaskell dsl because the
combinators for a dsl are in the dsl itself, but for Process it's the monad
typeclass.

Can think of the various derived typeclasses of Process as the shallow embedding
itself... override default defns? Cannot override IO

Paper: http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf
Look at figure 2

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

Step 1: each group of interface functions from the library (page 3 of paper) gets a typeclass,
parameterised over ProcessM
\begin{code}
-- Basic messaging
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
  receiveWait    :: [MatchM q ()] -> thingy q
  receiveTimeout :: Int -> [MatchM q ()] -> thingy (Maybe q)
  match          :: Serializable a => (a -> thingy q) ->MatchM q ()
  matchIf        :: Serializable a => (a -> Bool) -> (a -> thingy q) -> thingy q ()
  matchUnknown   :: thingy q -> MatchM q ()

-- Process management
class ProcMan thingy where
  spawn       :: NodeId -> Closure (thingy ()) -> thingy ProcessId
  call        :: Serializable a => NodeId -> Closure (thingy a) -> thingy a
  terminate   :: thingy a
  getSelfPid  :: thingy ProcessId
  getSelfNode :: thingy NodeId

-- Process monitoring
class ProcMon thingy where
  linkProcess :: ProcessId -> thingy ()
  monitorProcess :: ProcessId -> ProcessId -> MonitorAction -> thingy ()

-- Initialization
class Init thingy where
  -- type RemoteTable = [(String,Dynamic)]
  runRemote :: Maybe FilePath -> [RemoteTable] -> ( String -> thingy ()) -> IO ()
  -- type PeerInfo = Map String [NodeId]
  getPeers :: thingy PeerInfo
  findPeerByRole :: PeerInfo -> String -> [NodeId]

-- Logging
class Log thingy where
  say :: String -> thingy ()
\end{code}

-----------------------

Step 2. Instances for ProcessM are just the library definitions

> instance Basic ProcessM where
>   send    = CH.send
>   expect  = CH.expect

> instance Chan ProcessM where
>   newChan          = CH.newChan
>   sendChan         = CH.sendChan
>   receiveChan      = CH.receiveChan
>   mergePortsBiased = CH.mergePortsBiased
>   mergePortsRR     = CH.mergePortsRR

> instance Msg ProcessM where
>   receiveWait    = CH.receiveWait
>   receiveTimeout = CH.receiveTimeout
>   match          = CH.match
>   matchIf        = CH.matchIf
>   matchUnknown   = CH.matchUnknown

> -- Process management
> class ProcMan ProcessM where
>   spawn          = CH.spawn
>   call           = CH.call
>   terminate      = CH.terminate
>   getSelfPid     = CH.getSelfPid
>   getSelfNode    = CH.getSelfNode

> -- Process monitoring
> class ProcMon ProcessM where
>   linkProcess     = CH.linkProcess
>   monitorProcess  = CH.monitorProcess

> -- Initialization
> class Init ProcessM where
>   runRemote      = CH.runRemote
>   getPeers       = CH.getPeers
>   findPeerByRole = CH.findPeerByRole

> -- Logging
> class Log ProcessM where
>   say = = CH.say
-----------------------

Should be able to do this:

ping :: (Basic ProcessM, Chan ProcessM, Msg ProcessM ...) => ProcessM ()
ping = do { Pong partner <-expect
          ; self <- getSelfPid
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

\end{document}
