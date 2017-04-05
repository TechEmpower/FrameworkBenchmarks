#!/bin/bash

fw_depends dlang dub

# Clean any files from last run
rm -f collieHttp
rm -rf .dub
rm -f dub.selections.json

dub build -f -b release

./collieHttp &
