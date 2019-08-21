#!/usr/bin/env bash
#
# Generates a draft HPC .txt overlay file for 100% coverage.

readonly PROJECT='raindrop'
readonly SUITE='raindrop-test'

readonly DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
readonly TIX_FILE=$(stack path --local-hpc-root)/$PROJECT/$SUITE/$SUITE.tix
readonly HPC_DIR=$DIR/$(stack path --dist-dir)/hpc
readonly SRC=.

stack exec hpc -- draft --hpcdir="$HPC_DIR" --srcdir=$SRC "$TIX_FILE"
