#!/bin/bash
fw_depends dotnetcore

threadCount=$2
if [ "$threadCount" -lt "1" ]
then
    threadCount=1
fi

cd Benchmarks
cp appsettings.postgresql.json appsettings.json
sed -i 's|{db_server_placeholder}|'"${DBHOST}"'|g' appsettings.json
dotnet restore
dotnet publish --configuration Release --output bin/Release/publish

dotnet bin/Release/publish/Benchmarks.dll server.urls=http://*:8080 scenarios=$1 server=kestrel threadCount=$threadCount NonInteractive=true &
