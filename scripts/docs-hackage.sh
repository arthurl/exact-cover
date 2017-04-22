#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# KIV 1: Move haddock docs generation to stack when stack implementation is
# complete. See https://github.com/commercialhaskell/stack/issues/737
#
# KIV 2: Use `cabal upload` or `stack upload` when they support uploading
# candidate packages.
#
# This script was modified from
# https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh (see older
# commits).

# Check that user is specified
if [ "$#" -ne 1 ]; then
  echo "Usage: scripts/hackage-docs.sh HACKAGE_USER"
  exit 1
fi
user=$1

# Detect cabal file
cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

# Detect package name and version
pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")
if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi
if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi
echo "Detected package: $pkg-$ver"

# Temp dir to build docs in
dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source

# Upload documentation as candidate
curl -X PUT \
     -u "$user" \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/candidate/docs"
