#!/bin/bash

fw_depends mongodb dlang

# Clean any files from last run
rm -f fwb
rm -rf .dub

dub upgrade

dub build -b release --combined

./fwb &
