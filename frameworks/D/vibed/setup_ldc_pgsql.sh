#!/bin/bash

fw_depends postgresql dlang

# Clean any files from last run
rm -f fwb
rm -rf .dub

dub upgrade --verbose

dub build -b release --compiler=ldc2 --combined --config=postgresql

./fwb &
