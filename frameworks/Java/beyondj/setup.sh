#!/bin/bash

WORKING_DIR=beyondj-launcher/deploy

if [ ! -d "$WORKING_DIR" ]; then
        mkdir $WORKING_DIR
fi

cd beyondj-launcher/deploy/
fw_get -O http://beyondj.com/beyondj.jar
cd ../../

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

