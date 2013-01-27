#!/bin/sh

set -eu

cabal_install () {
  cabal install --force-reinstalls --ghc-options=-fhpc --disable-library-profiling --disable-documentation
}

if [ "${1:-""}" = '--deps' ]
then
  (cd .. && cabal_install)
  (cd ../test-framework-smallcheck && cabal_install)
fi

rm -f test.tix
ghc -fforce-recomp -fhpc test.hs
./test
hpc markup --srcdir=. --srcdir=../ --srcdir=../test-framework-smallcheck --destdir=html test.tix
