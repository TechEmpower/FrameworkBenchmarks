#!/bin/bash

fw_depends dlang dub

# Clean any files from last run
rm -f fwb
rm -rf .dub

dub build -b release

./fwb &
