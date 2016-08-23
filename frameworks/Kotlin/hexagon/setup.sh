#!/bin/bash

fw_depends java

./gradlew
nohup build/hexagon/bin/hexagon &
