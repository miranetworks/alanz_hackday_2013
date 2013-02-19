Initially working through http://izbicki.me/blog/nuclear-weapon-statistics-using-monoids-groups-and-modules-in-haskell

Before starting, make sure GHC-7.6.1 is in your path

```bash
virthualenv --ghc=/home/alanz/Downloads/haskell/ghc-7.6.1-i386-unknown-linux.tar.bz2
```

Then, to start working, from this directory

```bash
. .virthualenv/bin/activate
```

OR

```bash
cabal install cabal-dev
cabal-dev update
cabal-dev install HLearn-algebra
cabal-dev install HLearn-distributions
cabal-dev install HLearn-classification # Fails?
cabal-dev install cassava
cabal-dev install lens
```
