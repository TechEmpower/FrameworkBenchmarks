#!/bin/bash

fw_depends java7 resin maven

sed -i 's|db.ConnectString = .*/|db.ConnectString = '"$DBHOST"':3306/|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|root-directory=".*/FrameworkBenchmarks/frameworks/Java/gemini|root-directory="'"$TROOT"'|g' Docroot/WEB-INF/resin.xml
sed -i 's|db.Driver.Class = .*|db.Driver.Class = com.mysql.jdbc.Driver|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|db.Driver.UrlPrefix = .*|db.Driver.UrlPrefix = jdbc:mysql://|g' Docroot/WEB-INF/GeminiHello.conf
mkdir -p Docroot/WEB-INF/classes
ant compile
resinctl -conf $TROOT/Docroot/WEB-INF/resin.xml start
