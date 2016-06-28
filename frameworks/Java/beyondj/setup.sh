#!/bin/bash

fw_depends java

WORKING_DIR=beyondj-launcher/deploy

if [ ! -d "$WORKING_DIR" ]; then
        mkdir $WORKING_DIR
fi

cd beyondj-launcher/deploy/
fw_get -O http://beyondj.com/beyondj-launcher-1.0-SNAPSHOT.jar
cd ../../

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj-launcher-1.0-SNAPSHOT.jar system.platform.dbserver=${DBHOST} numInstances=10

