#!/bin/bash

fw_depends mongodb java

./gradlew
nohup build/hexagon/bin/hexagon &
