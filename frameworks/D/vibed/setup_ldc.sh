#!/bin/bash

fw_depends mongodb dlang

# Clean any files from last run
rm -f fwb
rm -rf .dub

dub upgrade --verbose

dub build -b release --compiler=ldc2 --combined --verbose

./fwb &
