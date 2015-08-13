# travis-meta-yaml

[![Build Status](https://travis-ci.org/phadej/travis-meta-yaml.svg?branch=master)](https://travis-ci.org/phadej/travis-meta-yaml)
[![Hackage](https://img.shields.io/hackage/v/travis-meta-yaml.svg)](http://hackage.haskell.org/package/travis-meta-yaml)

## Motivation

```
language: haskell-multi-ghc

env:
  - GHCVER=7.8.4 CABALVER=1.18
  - GHCVER=7.10.1 CABALVER=1.22
  - GHCVER=head CABALVER=1.22

matrix:
  fast_finish: true
  allow_failures:
    - env: GHCVER=head CABALVER=1.22

branches:
  only:
    - master
```

into

```
script:
- cabal configure --package-db=clear --package-db=global --package-db=$HOME/package-dbs/$GHCVER.d
  --enable-tests
- cabal build
- cabal test
branches:
  only:
  - master
matrix:
  fast_finish: true
  include:
  - env: GHCVER=7.8.4 CABALVER=1.18
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.18
        - ghc-7.8.4
        - libgmp-dev
  - env: GHCVER=7.10.1 CABALVER=1.22
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.22
        - ghc-7.10.1
        - libgmp-dev
  - env: GHCVER=head CABALVER=1.22
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.22
        - ghc-head
        - libgmp-dev
  allow_failures:
  - env: GHCVER=head CABALVER=1.22
install:
- cabal update
- sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config
- ./init-custom-pkg-db.sh $HOME/package-dbs/$GHCVER.d
cache:
  directories:
  - ~/package-dbs
  apt: true
before_install:
- export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
- echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo
  '?')]"
- cabal --version
language: c
sudo: false
```

## Features

Interpolate environment variables into `addons` in `matrix.include`:

```yaml
env:
  - CABALVER=1.18 GHCVER=7.8.4
  - CABALVER=1.22 GHCVER=7.10.1
  - CABALVER=1.22 GHCVER=head

addons:
  apt:
    sources:
      - hvr-ghc
    packages:
      - cabal-install-$CABALVER
      - ghc-$GHCVER

matrix:
  allow_failures:
    - env: CABALVER=1.22 GHCVER=head
```

into

```yaml
matrix:
  include:
  - env: CABALVER=1.18 GHCVER=7.8.4
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.18
        - ghc-7.8.4
  - env: CABALVER=1.22 GHCVER=7.10.1
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.22
        - ghc-7.10.1
  - env: CABALVER=1.22 GHCVER=head
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - cabal-install-1.22
        - ghc-head
  allow_failures:
  - env: CABALVER=1.22 GHCVER=head
```

### Other

Any feature requests are welcome.
