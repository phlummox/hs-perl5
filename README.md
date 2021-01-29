# hs-perl5

![Linux Build Status](https://github.com/phlummox/hs-perl5/workflows/haskell-build/badge.svg)

This package provides a Haskell interface to an embedded Perl 5 interpreter.

Documentation and functionality is scarce at the moment; see the tests
under `test` for some basic usage examples.

Currently supported features are:

* Function calls
* Method calls
* Module imports
* Callbacks

**NB: This package is experimental and still in development**

## Prerequisites

This package requires you to have the Perl libraries and header files
installed on your system (at least 5.8.8).

On a Debian-based system, they can typically be installed by typing

```
$ sudo apt-get install perl-dev
```

## Installation

This package can be installed either using [Stack][stack] or
[cabal][cabal]. First, `git clone` this repository, `cd` in, and then

```
$ stack --stack-yaml=stack-lts-11.yaml build
# or use one of the other provided stack.yaml files
```

or

```
$ cabal update
$ cabal install --dependencies-only
$ cabal build
```

Currently only `cabal` versions from >= 1.24 to < 2.2 will work.
It is intended to extend this to more versions in future.

[stack]: https://www.haskellstack.org/
[cabal]: https://www.haskell.org/cabal/
