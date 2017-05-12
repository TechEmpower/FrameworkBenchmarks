#!/bin/bash

fw_depends mongodb java

gradle/wrapper -x test

nohup build/install/hexagon/bin/hexagon &
