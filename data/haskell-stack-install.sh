# Based on https://github.com/hvr/multi-ghc-travis

# Update database
# cabal update

# we want one job
# sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config

# Create stack.yaml
if [ "$STACK_SOLVER" = "YES" ]; then
  rm -f stack.yaml
  stack update
  stack init --solver
else
  cp $STACK_YAML stack.yaml
fi

# Generate install-plan
stack --no-terminal --skip-ghc-check list-dependencies|sort > installplan.txt
cat installplan.txt

# Setup
stack --no-terminal --skip-ghc-check setup

# check whether current requested install-plan stack-work cache
if diff -u installplan.txt $HOME/.stack-work-cache/installplan.txt; then
  echo "cabal build-cache HIT"
  rm -rf .stack-work
  cp -a $HOME/.stack-work-cache .stack-work
  rm -f .stack-work/installplan.txt
else
  echo "cabal build-cache MISS"
  rm -rf $HOME/.stack-work-cache
  stack --no-terminal --skip-ghc-check build --test --only-dependencies
fi

# snapshot .stack-work on cache miss
if [ ! -d $HOME/.stack-work-cache ]; then
  echo "snapshotting package-db to build-cache"
  cp -a .stack-work $HOME/.stack-work-cache
  cp -a installplan.txt $HOME/.stack-work-cache/installplan.txt
fi
