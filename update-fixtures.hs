#!/bin/sh

set -e
set -x

TML=.stack-work/install/x86_64-osx/lts-2.21/7.8.4/bin/travis-meta-yaml

$TML generate -i .travis.meta.yml -o .travis.yml

for FIXTURE in multi-ghc stack multi without-matrix; do
  $TML generate -i fixtures/$FIXTURE.yml -o fixtures/$FIXTURE-processed.yml
done
