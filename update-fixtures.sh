#!/bin/sh

set -e
set -x

TML=$(stack path --local-install-root)/bin/travis-meta-yaml

$TML generate -i .travis.meta.yml -o .travis.yml

for FIXTURE in multi-ghc stack multi without-matrix; do
  $TML generate -i fixtures/$FIXTURE.yml -o fixtures/$FIXTURE-processed.yml
done
