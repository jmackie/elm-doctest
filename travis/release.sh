#!/usr/bin/env bash

set -e

# This is the default output name of `nix-build`, but doesn't
# hurt to be explicit
BUILD=result
# Temporary directory that will eventually become the release tarball
DIST=$(mktemp -d)
# Name of the release tarball
TARBALL=${TARBALL:-release.tar.gz}

nix-build -o "$BUILD"
cp --recursive --dereference --no-preserve=ownership "$BUILD" "$DIST"
tar -zcvf "$TARBALL" "$DIST"
