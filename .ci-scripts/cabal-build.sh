#!/usr/bin/env bash

# for use on circle-ci

set -euo pipefail
set -x

mkdir -p "$HOME/.local/bin"
PATH=$HOME/.local/bin:$HOME/.cabal/bin:$PATH

# check we have cabal and ghc
cabal --version
ghc --version

# cabal prep
# -  Use S3 mirror of Hackage
mkdir -p "$HOME/.cabal"
echo "remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/" > "${HOME}/.cabal/config"
echo "remote-repo-cache: $HOME/.cabal/packages" >> "${HOME}/.cabal/config"

cabal new-update
cabal new-configure --enable-tests
cabal new-build --enable-tests --dependencies-only
cabal new-install hspec-discover --symlink-bindir=$HOME/.local/bin
cabal new-build --enable-tests
cabal new-test --jobs=1
