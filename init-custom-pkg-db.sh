#!/bin/sh

set -e
set -x

if [ -z "$1" ]; then
    echo "Usage: ./init-custom-pkg-db.sh PKGDB"
    exit 1
fi

PKGDB=$1
TMP_LIST=`mktemp /tmp/package-list.XXXXXX`

# Generate install plan
cabal install --only-dependencies --enable-tests --dry-run > $TMP_LIST

if diff -u $PKGDB/packages.txt $TMP_LIST; then
    echo "Dependencies are in place"
else
	# Create new package db
    rm -rf $PKGDB
    mkdir -p `dirname $PKGDB`
    ghc-pkg init $PKGDB

	# Install dependencies there
    cabal install \
        --enable-tests --only-dependencies \
        --package-db=clear --package-db=global --package-db=$PKGDB \
        --libdir=$PKGDB/lib --haddockdir=$PKGDB/doc \
        --disable-documentation --disable-library-profiling

	# If succeed, "tag" with list of packages
    mv $TMP_LIST $PKGDB/packages.txt
fi
