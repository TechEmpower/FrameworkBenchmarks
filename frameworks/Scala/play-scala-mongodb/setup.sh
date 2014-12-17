#!/bin/bash

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

${PLAY2_HOME}/play clean dist

cd target/universal
unzip play-scala-mongodb-1.0-SNAPSHOT.zip
cd play-scala-mongodb-1.0-SNAPSHOT/bin
chmod +x play-scala-mongodb

./play-scala-mongodb &