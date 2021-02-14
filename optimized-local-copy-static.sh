#!/bin/bash

echo "Compiling View"
makeView
echo "Compiling Save"
makeSave
echo "Compiling Config"
makeConfig

optimized-copy-static.sh '/tmp/minify-staging' '/Volumes/Work/projects/code/haskell/toy/slate/static/'