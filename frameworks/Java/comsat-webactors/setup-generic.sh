#!/bin/bash

fw_depends java8

$TROOT/gradlew capsule

CAPSULE=`ls build/libs/comsat-webactors-*-capsule.jar`

java -Dcapsule.mode=$MODE -jar $CAPSULE
