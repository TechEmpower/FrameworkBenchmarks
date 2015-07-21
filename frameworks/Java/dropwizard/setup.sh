#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

sed -i 's|url: jdbc:mysql://.*/hello_world|url: jdbc:mysql://'"${DBHOST}"':3306/hello_world|g' hello-world.yml

mvn clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world.yml &