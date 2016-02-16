# distributed-systems-haskell

# Run

Configure stack

```
stack setup
```

Build

```
stack build
stack exec chscheck
```

# TODO
1. Literature review, write background and method paper
2. Collect 3-4 interesting examples in this repo + refactor them to support
   QuickCheck


# Topics mentioned in discussion that we should look into
- Consensus Protocols
- [Cloud Haskell](http://haskell-distributed.github.io/)
- Concurrent Programming in Erlang
- Aphyr Jepsen
- Building Quick Check support on Distributed Systems
- Parameterise the example programs by a typeclass, then provide different instances for (a) actual running (b) testing/loggint/etc
- perhaps this would be useful: https://hackage.haskell.org/package/MuCheck-QuickCheck

# References
- [Towards Haskell in the Cloud](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf)
- [Making reliable distributed systems in the presence of software errors](http://ftp.nsysu.edu.tw/FreeBSD/ports/distfiles/erlang/armstrong_thesis_2003.pdf) -- On building systems in Erlang (chapter 2)
