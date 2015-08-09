# travis-meta-yaml

[![Build Status](https://travis-ci.org/phadej/travis-meta-yaml.svg?branch=master)](https://travis-ci.org/phadej/travis-meta-yaml)
[![Hackage](https://img.shields.io/hackage/v/travis-meta-yaml.svg)](http://hackage.haskell.org/package/travis-meta-yaml)

## Motivation

Turn

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

## Features

Currently only expanding of `addons.apt`. Any feature requests are welcome.
