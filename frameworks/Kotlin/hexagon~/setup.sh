#!/bin/bash

fw_depends java

./gradlew
nohup build/install/hexagon/bin/hexagon &
