#!/bin/bash

fw_depends java7 maven

mvn clean package
cd target
java -jar beyondj.jar &
