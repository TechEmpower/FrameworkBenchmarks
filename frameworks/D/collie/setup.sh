#!/bin/bash

fw_depends dlang

# Clean any files from last run
rm -f http
rm -rf .dub
rm -f dub.selections.json

dub build -f -b release

./http &
