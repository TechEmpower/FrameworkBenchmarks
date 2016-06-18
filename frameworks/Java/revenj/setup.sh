#!/bin/bash

fw_depends java resin maven mono

source $IROOT/java.installed

echo "Cleaning up..."
rm -rf $TROOT/tmp $TROOT/model $TROOT/revenj.java $TROOT/dsl-clc.jar $TROOT/dsl-compiler.zip $TROOT/dsl-compiler.exe $TROOT/revenj-java-0.8.zip

echo "Download DSL compiler client"
wget -O $TROOT/dsl-clc.jar https://github.com/ngs-doo/dsl-compiler-client/releases/download/1.5.0/dsl-clc.jar

echo "Download compatible DSL compiler"
wget -O $TROOT/dsl-compiler.zip https://github.com/ngs-doo/revenj/releases/download/1.2.1/dsl-compiler.zip

echo "Unzipping DSL compiler"
unzip $TROOT/dsl-compiler.zip -d $TROOT

echo "Prepare Revenj 0.8 dependencies"
wget -O $TROOT/revenj-java-0.8.zip https://github.com/ngs-doo/revenj/releases/download/1.2.1/revenj-java-0.8.zip
unzip $TROOT/revenj-java-0.8.zip -d $TROOT/revenj.java

echo "Compiling the server model and downloading dependencies..."
java -jar $TROOT/dsl-clc.jar \
	temp=$TROOT/tmp/ \
	force \
	dsl=$TROOT/src \
	manual-json \
	compiler=$TROOT/dsl-compiler.exe \
	namespace=dsl \
	revenj.java=$TROOT/model/gen-model.jar \
	no-prompt \
	download

echo "Adding model to local Maven repository..."
mvn deploy:deploy-file \
	-Durl=file://model \
	-Dfile=model/gen-model.jar \
	-DgroupId=dsl \
	-DartifactId=gen-model \
	-Dpackaging=jar \
	-Dversion=1.0

echo "Changing the database"
cat $TROOT/web.xml | sed 's/localhost/'$DBHOST'/g' > $TROOT/src/main/webapp/WEB-INF/web.xml
	
mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/revenj.war $RESIN_HOME/webapps/
JAVA_EXE=$JAVA_HOME/bin/java resinctl start
