#!/usr/bin/env sh

set -e
set -x


TEST_DIR="test/modules/original"

cabal run -- -o test.html --html \
    compare "$TEST_DIR/Test.hs" "$TEST_DIR/TestChangeAll.hs"
