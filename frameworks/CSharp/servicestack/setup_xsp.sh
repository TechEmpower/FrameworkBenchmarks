#!/bin/bash

fw_depends nginx xsp mono

sed -i 's|localhost|'"$DBHOST"'|g' src/Web.config
# extra cleaning
rm -rf src/bin src/obj
xbuild src/ServiceStackBenchmark.csproj /t:Clean
xbuild src/ServiceStackBenchmark.csproj /p:Configuration=Release
# xsp
MONO_OPTIONS=--gc=sgen xsp4 --port 8080 -nonstop &
