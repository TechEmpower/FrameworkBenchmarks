#!/bin/bash

fw_depends java

./gradlew
build/install/hexagon/bin/hexagon >/dev/null 2>/dev/null &
