#!/bin/bash

elm make src/${1}.elm --optimize --output="${2}.js"