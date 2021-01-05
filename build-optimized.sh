#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Please supply <name of Elm file> <name of js file>"
    echo "extensions should be omitted in either case"
    echo "eg. build-optimized.sh Save scrib-save"
    exit 1
fi

elm make src/${1}.elm --optimize --output="resources/js/${2}.js"