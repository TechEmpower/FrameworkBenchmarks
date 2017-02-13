#!/bin/bash

fw_depends mongodb java

gradle/wrapper

nohup build/install/hexagon/bin/hexagon &
