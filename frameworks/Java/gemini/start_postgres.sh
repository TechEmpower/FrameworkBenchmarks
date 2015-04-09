#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

sed -i 's|db.ConnectString = .*/|db.ConnectString = '"$DBHOST"':5432/|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|root-directory=".*/FrameworkBenchmarks/frameworks/Java/gemini|root-directory="'"$TROOT"'|g' Docroot/WEB-INF/resin.xml
sed -i 's|db.Driver.Class = .*|db.Driver.Class = org.postgresql.Driver|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|db.Driver.UrlPrefix = .*|db.Driver.UrlPrefix = jdbc:postgresql://|g' Docroot/WEB-INF/GeminiHello.conf
mkdir -p Docroot/WEB-INF/classes
ant compile
$RESIN_HOME/bin/resinctl -conf $TROOT/Docroot/WEB-INF/resin.xml start
