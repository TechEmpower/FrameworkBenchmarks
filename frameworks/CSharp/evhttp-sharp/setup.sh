#!/bin/bash

fw_depends mono

#extra cleaning
rm -rf src/bin src/obj

xbuild src/EvHttpSharpBenchmark.csproj /t:Clean
xbuild src/EvHttpSharpBenchmark.csproj /p:Configuration=Release

export MONO_GC_PARAMS=nursery-size=64m

mono -O=all $TROOT/src/bin/Release/EvHttpSharpBenchmark.exe 127.0.0.1 8085 $CPU_COUNT &
