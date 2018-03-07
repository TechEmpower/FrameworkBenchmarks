#!/usr/bin/env bash

fw_depends java8

$TROOT/gradlew clean capsule

CAPSULE=`ls build/libs/comsat-*-capsule.jar`
java -Dcapsule.mode=$MODE -jar $CAPSULE
