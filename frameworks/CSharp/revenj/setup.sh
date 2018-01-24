#!/bin/bash

fw_depends postgresql java mono dsl_platform

echo "Cleaning up..."
rm -rf $TROOT/exe $TROOT/tmp $TROOT/dsl-clc.jar $TROOT/http-server.zip

echo "Download DSL compiler client"
wget -O $TROOT/dsl-clc.jar https://github.com/ngs-doo/dsl-compiler-client/releases/download/1.8.2/dsl-clc.jar

echo "Download Revenj.NET HTTP server 1.4.2"
wget -O $TROOT/http-server.zip https://github.com/ngs-doo/revenj/releases/download/1.4.2/http-server.zip

echo "Unzipping HTTP server"
unzip $TROOT/http-server.zip -d $TROOT/exe

echo "Compiling the server model and downloading dependencies..."
java -jar $TROOT/dsl-clc.jar \
	temp=$TROOT/tmp/ \
	force \
	dsl=$TROOT/Revenj.Bench \
	manual-json \
	compiler=$IROOT/dsl-compiler.exe \
	revenj.net=$TROOT/exe/ServerModel.dll \
	no-prompt \
	dependencies:revenj.net=$TROOT/exe

echo "Compiling the benchmark project..."
xbuild $TROOT/Revenj.Bench/Revenj.Bench.csproj /t:Rebuild /p:Configuration=Release

echo "Copying the configuration template"
cat $TROOT/Revenj.Http.exe.config | sed 's|\(ConnectionString.*server=\)localhost|\1'"${DBHOST}"'|' > $TROOT/exe/Revenj.Http.exe.config

echo "Running the Revenj instance"
mono $TROOT/exe/Revenj.Http.exe
