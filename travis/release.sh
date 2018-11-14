#!/usr/bin/env bash

set -e

# This is the default output name of `nix-build`, but doesn't
# hurt to be explicit
BUILD=result
# Temporary directory that will eventually become the release tarball
DIST=$(mktemp -d)
# Name of the release tarball
TARBALL=${TARBALL:-release.tar.gz}

# Need to get rid of any lingering environment file(s)
rm .ghc.environment.*

nix-build -o "$BUILD" default.nix
cp --verbose --no-preserve=ownership,mode "$BUILD/bin/elm-doctest" "$DIST"
chmod 0755 "$DIST/elm-doctest"
tar zcvfC "$TARBALL" "$DIST" elm-doctest
