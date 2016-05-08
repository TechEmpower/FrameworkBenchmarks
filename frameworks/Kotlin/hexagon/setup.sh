#!/bin/bash

fw_depends java8

export JAVA_HOME=/opt/java8
gradlew
build/install/hexagon/bin/hexagon >/dev/null 2>/dev/null &
