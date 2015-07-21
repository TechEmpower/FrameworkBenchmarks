#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' config/dev/environment.properties
sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' config/dev/environment.properties

mvn clean package
mvn assembly:single
cd target
unzip world-1.0-SNAPSHOT-zip-with-dependencies.zip
cd world-1.0-SNAPSHOT
java -jar world-1.0-SNAPSHOT.jar &