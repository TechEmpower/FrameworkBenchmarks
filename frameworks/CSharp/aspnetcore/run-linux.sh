#!/bin/bash

fw_depends mono
sudo apt-get install unzip libunwind8 -y

if [ -d test ]; then
  rm -rf test
fi

mkdir test
cd test
git clone https://github.com/aspnet/benchmarks.git

cd benchmarks
source ./build.sh || true
export PATH=$PATH:$HOME/.dotnet
cd src/Benchmarks
dotnet restore
dotnet build -c Release -f netcoreapp1.0
dotnet run -c Release server.urls=http://*:8080 scenarios=$1 server=kestrel threadCount=8 NonInteractive=true &