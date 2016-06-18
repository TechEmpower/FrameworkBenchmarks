#!/bin/bash

fw_depends java mono

echo "Cleaning up..."
rm -rf $TROOT/exe $TROOT/tmp $TROOT/dsl-clc.jar $TROOT/http-server.zip  $TROOT/dsl-compiler.zip $TROOT/dsl-compiler.exe

echo "Download DSL compiler client"
wget -O $TROOT/dsl-clc.jar https://github.com/ngs-doo/dsl-compiler-client/releases/download/1.5.0/dsl-clc.jar

echo "Download Revenj.NET HTTP server 1.2.1"
wget -O $TROOT/http-server.zip https://github.com/ngs-doo/revenj/releases/download/1.2.1/http-server.zip

echo "Unzipping HTTP server"
unzip $TROOT/http-server.zip -d $TROOT/exe

echo "Download DSL compiler for Revenj.NET 1.2.1"
wget -O $TROOT/dsl-compiler.zip https://github.com/ngs-doo/revenj/releases/download/1.2.1/dsl-compiler.zip

echo "Unzipping DSL compiler"
unzip $TROOT/dsl-compiler.zip -d $TROOT

echo "Compiling the server model and downloading dependencies..."
java -jar $TROOT/dsl-clc.jar \
	temp=$TROOT/tmp/ \
	force \
	dsl=$TROOT/Revenj.Bench \
	manual-json \
	compiler=$TROOT/dsl-compiler.exe \
	revenj.net=$TROOT/exe/ServerModel.dll \
	no-prompt \
	dependencies:revenj.net=$TROOT/exe \
	download

echo "Compiling the benchmark project..."
xbuild $TROOT/Revenj.Bench/Revenj.Bench.csproj /t:Rebuild /p:Configuration=Release

echo "Copying the configuration template"
cat $TROOT/Revenj.Http.exe.config | sed 's|\(ConnectionString.*server=\)localhost|\1'"${DBHOST}"'|' > $TROOT/exe/Revenj.Http.exe.config

echo "Running the Revenj instance"
mono $TROOT/exe/Revenj.Http.exe
sleep 5
