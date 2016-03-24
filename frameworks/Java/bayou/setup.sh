#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single
cd target
java -jar bayou_TFB-0.1-jar-with-dependencies.jar &
