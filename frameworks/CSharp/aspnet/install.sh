#!/bin/bash

fw_depends nginx mono xsp

wget -q -N http://nuget.org/nuget.exe -O ${TROOT}/lib/.nuget/NuGet.exe
