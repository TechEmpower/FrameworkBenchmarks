#!/bin/bash

sed -i 's/db.ConnectString = .*:3306/db.ConnectString = '"$TFB_DATABASE_HOST"':3306/g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|root-directory=".*/FrameworkBenchmarks/gemini|root-directory="'"$TROOT"'|g' Docroot/WEB-INF/resin.xml
mkdir -p Docroot/WEB-INF/classes
ant compile
$RESIN_HOME/bin/resinctl -conf $TROOT/Docroot/WEB-INF/resin.xml start