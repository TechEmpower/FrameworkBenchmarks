#!/bin/bash

source $IROOT/dlang.installed
source $IROOT/dub.installed

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' source/app.d

# Clean any files from last run
rm -f fwb
rm -rf .dub

dub build --force

./fwb &
