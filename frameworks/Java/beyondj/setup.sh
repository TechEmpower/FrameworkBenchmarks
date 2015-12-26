#!/bin/bash

fw_depends java8 maven

FILE=beyondj-launcher/deploy/beyondj.jar

rm -rf results
mkdir results

if [ -f $FILE ];
then
        echo "File $FILE exists"
else
	cd beyondj-launcher/deploy/
	jar -cvf0M beyondj.jar META-INF *
	cd ../../
fi

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

