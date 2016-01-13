#!/bin/bash

fw_depends java8 maven

FILE=beyondj-launcher/deploy/beyondj.jar

rm -rf results
mkdir results

cd beyondj-launcher/deploy/
wget http://beyondj.com/beyondj.jar
chmod 775 beyondj.jar
cd ../../

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

