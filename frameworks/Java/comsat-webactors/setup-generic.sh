#!/usr/bin/env bash

fw_depends java

$TROOT/gradlew clean capsule

CAPSULE=`ls build/libs/comsat-webactors-*-capsule.jar`
java -Dcapsule.mode=$MODE -jar $CAPSULE
