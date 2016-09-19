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

# Cabal config
if [ -n "$CABALCONFIG" ]; then
  cp $CABALCONFIG cabal.config
fi

# Stackage snapshot
if [ -n "$STACKAGESNAPSHOT" ]; then
  curl --silent https://www.stackage.org/$STACKAGESNAPSHOT/cabal.config | grep -v "$(cabal info . -v0 | head -n 1 | awk '{ print $2 }' | sed -E 's/-[0-9]+(\.[0-9]+)+//') ==" > cabal.config
fi

# Generate install-plan
cabal install --constraint='integer-simple installed' --only-dependencies $CABALCONFOPTS --dry -v > installplan.txt
sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt

# check whether current requested install-plan matches cached package-db snapshot
if diff -u $HOME/.cabsnap/installplan.txt installplan.txt; then
  echo "cabal build-cache HIT"
  rm -rfv .ghc
  cp -av $HOME/.cabsnap/ghc $HOME/.ghc
  cp -av $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/
else
  echo "cabal build-cache MISS"
  rm -rf $HOME/.cabsnap
  mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin

  cabal install --constraint='integer-simple installed' --only-dependencies $CABALCONFOPTS
fi

# snapshot package-db on cache miss
if [ ! -d $HOME/.cabsnap ]; then
  echo "snapshotting package-db to build-cache"
  mkdir $HOME/.cabsnap
  cp -av $HOME/.ghc $HOME/.cabsnap/ghc
  cp -av $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/
fi
