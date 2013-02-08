#!/bin/sh

set -eu

cabal_install () {
  cabal clean
  cabal install --force-reinstalls --ghc-options=-fhpc --disable-library-profiling --disable-documentation
}

if [ "${1:-""}" = '--deps' ]
then
  (cd .. && cabal_install)
  (cd ../test-framework-smallcheck && cabal_install)
fi

rm -f test.tix
cabal configure
cabal build
if [ "${1:-""}" = '--ci' ]
then
  ./dist/build/smallcheck-test/smallcheck-test --jxml=j.xml
else
  ./dist/build/smallcheck-test/smallcheck-test
fi
hpc markup --srcdir=. --srcdir=../ --srcdir=../test-framework-smallcheck --destdir=html smallcheck-test.tix
