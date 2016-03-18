#!/bin/bash

fw_depends java

$TROOT/gradlew clean capsule

CAPSULE=`ls build/libs/comsat-servlet-*-capsule.jar`
java -jar $CAPSULE
