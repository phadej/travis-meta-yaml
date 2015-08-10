# travis-meta-yaml

[![Build Status](https://travis-ci.org/phadej/travis-meta-yaml.svg?branch=master)](https://travis-ci.org/phadej/travis-meta-yaml)
[![Hackage](https://img.shields.io/hackage/v/travis-meta-yaml.svg)](http://hackage.haskell.org/package/travis-meta-yaml)

## Motivation

```
language: haskell-stack

env:
  - GHCVER=7.8.4 STACKYAML=stack-7.8.4.yaml
  - GHCVER=7.10.1 STACKYAML=stack-7.10.yaml
```

into

```
script:
- stack +RTS -N2 -RTS --no-terminal --skip-ghc-check --stack-yaml $STACKYAML test
matrix:
  include:
  - env: GHCVER=7.8.4 STACKYAML=stack-7.8.4.yaml
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - ghc-7.8.4
        - libgmp-dev
  - env: GHCVER=7.10.1 STACKYAML=stack-7.10.yaml
    addons:
      apt:
        sources:
        - hvr-ghc
        packages:
        - ghc-7.10.1
        - libgmp-dev
install:
- stack +RTS -N2 -RTS --no-terminal --skip-ghc-check --stack-yaml $STACKYAML setup
- stack +RTS -N2 -RTS --no-terminal --skip-ghc-check --stack-yaml $STACKYAML test
  --only-snapshot
cache:
  directories:
  - ~/.stack
  apt: true
before_install:
- export PATH=/opt/ghc/$GHCVER/bin:$PATH
- export PATH=~/.local/bin:$PATH
- if [ ! -e ~/.local/bin/stack ]; then mkdir -p ~/.local/bin; travis_retry curl -L
  https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-linux.gz
  | gunzip > ~/.local/bin/stack; chmod a+x ~/.local/bin/stack; fi
- echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo
  '?')]"
- stack +RTS -N2 -RTS --version
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
