#!/bin/bash

sed -i 's|db.ConnectString = .*/|db.ConnectString = '"$DBHOST"':3306/|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|db.Driver.Class = .*|db.Driver.Class = com.mysql.jdbc.Driver|g' Docroot/WEB-INF/GeminiHello.conf
sed -i 's|db.Driver.UrlPrefix = .*|db.Driver.UrlPrefix = jdbc:mysql://|g' Docroot/WEB-INF/GeminiHello.conf

docker build -t gemini .

docker run -d --rm -p 8080:8080 gemini