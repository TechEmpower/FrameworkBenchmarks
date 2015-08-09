#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

sed -i 's|###DBHOST*|'"${DBHOST}"'|g' src/main/resources/application.yaml

mvn clean package -Dmaven.test.skip=true

java -jar -Xmx2048m -Xms1024m target/mangooioapp.jar &