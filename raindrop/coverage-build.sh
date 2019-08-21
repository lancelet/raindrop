#!/usr/bin/env bash
#
# Runs a coverage build with overlay.

readonly DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd "$DIR" > /dev/null

readonly TIX_FILE=$(stack path --local-hpc-root)/$PROJECT/$SUITE/$SUITE.tix
readonly HPC_DIR=$(stack path --dist-dir)/hpc
readonly SRC=.
readonly EXTRA_TIX_FILES=$(stack path --local-hpc-root)/extra-tix-files

stack test --no-terminal --coverage --haddock --no-haddock-deps

readonly PKG_NAME=$(ls "$HPC_DIR" | grep raindrop)
readonly HASH=$(echo $PKG_NAME | sed 's/raindrop-[0-9.]*-\([0-9A-Za-z]*\)/\1/g')
readonly TEMP_DIR=$(mktemp -d)
readonly OVERLAY_FILE=$TEMP_DIR/coverage-overlay.txt
readonly TIX_OVERLAY=$TEMP_DIR/coverage-overlay.tix
cat coverage-overlay.txt | sed "s/{{PACKAGE_HASH}}/$HASH/g" > $OVERLAY_FILE

stack exec hpc -- overlay --hpcdir="$HPC_DIR" --srcdir=$SRC "$OVERLAY_FILE" > "$TIX_OVERLAY"

stack hpc report raindrop "$TIX_OVERLAY"
