#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Please supply <name_of_elm_file_without_extension> <name_of_js_file_without_extension>"
    echo "eg. build-optimized.sh Save scrib-save"
    exit 1
fi

elm make src/${1}.elm --optimize --output="resources/js/${2}.js"