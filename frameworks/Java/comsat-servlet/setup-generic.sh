#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn package

cd target
CAPSULE=`ls comsat-servlet-*-fat.jar`
java -Dcapsule.mode=$MODE -jar $CAPSULE
