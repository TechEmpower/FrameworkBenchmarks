#!/bin/bash

fw_depends java8 maven

FILE=beyondj-launcher/deploy/beyondj.jar

if [ -f $FILE ];
then
        echo "File $FILE exists"
        rm beyondj-launcher/deploy/beyondj.jar
fi

cd beyondj-launcher/deploy/
jar -cvf0M beyondj.jar META-INF *
cd ../../

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

