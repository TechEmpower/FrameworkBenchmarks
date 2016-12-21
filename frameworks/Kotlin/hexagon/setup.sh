#!/bin/bash

fw_depends mongodb java

gradle/wrapper

nohup build/hexagon/bin/hexagon &
