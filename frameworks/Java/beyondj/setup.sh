#!/bin/bash

fw_depends java8

FILE=beyondj-launcher/deploy/beyondj.jar
DIRECTORY=results

if [ ! -d "$DIRECTORY" ]; then
	mkdir results
fi

cd beyondj-launcher/deploy/
fw_get http://beyondj.com/beyondj.jar
chmod 775 beyondj.jar
cd ../../

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

