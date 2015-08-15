# Based on https://github.com/hvr/multi-ghc-travis

# Package database
if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
   zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz > $HOME/.cabal/packages/hackage.haskell.org/00-index.tar
   ls -l $HOME/.cabal/packages/hackage.haskell.org/
fi

# Update database
cabal update

# we want one job
sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config

# Generate install-plan
cabal install --only-dependencies --enable-tests --enable-benchmarks --dry -v > installplan.txt
sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt

# check whether current requested install-plan matches cached package-db snapshot
if diff -u installplan.txt $HOME/.cabsnap/installplan.txt; then
  echo "cabal build-cache HIT"
  rm -rfv .ghc
  cp -av $HOME/.cabsnap/ghc $HOME/.ghc
  cp -av $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/
else
  echo "cabal build-cache MISS"
  rm -rf $HOME/.cabsnap
  mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin
  cabal install --only-dependencies --enable-tests --enable-benchmarks
fi

# snapshot package-db on cache miss
if [ ! -d $HOME/.cabsnap ]; then
  echo "snapshotting package-db to build-cache"
  mkdir $HOME/.cabsnap
  cp -av $HOME/.ghc $HOME/.cabsnap/ghc
  cp -av $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/
fi
