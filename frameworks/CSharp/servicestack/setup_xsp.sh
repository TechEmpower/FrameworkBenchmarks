#!/bin/bash

fw_depends mysql postgresql mongodb nginx mono

rm -rf lib/
mkdir lib

wget https://dist.nuget.org/win-x86-commandline/latest/nuget.exe -O nuget.exe

mono src/.nuget/nuget.exe install src/packages.config -OutputDirectory lib/

# extra cleaning
rm -rf src/bin src/obj
xbuild src/ServiceStackBenchmark.csproj /t:Clean
xbuild src/ServiceStackBenchmark.csproj /p:Configuration=Release
# xsp
MONO_OPTIONS=--gc=sgen xsp4 --port 8080 -nonstop &
