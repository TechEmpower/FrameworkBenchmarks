#!/bin/bash
fw_depends mono dotnetcore
sudo apt-get install unzip libunwind8 -y

cd Benchmarks
dotnet restore
dotnet build -c Release -f netcoreapp1.0
dotnet run -c Release server.urls=http://*:8080 scenarios=$1 server=kestrel threadCount=1 NonInteractive=true &
