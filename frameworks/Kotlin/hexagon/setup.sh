#!/bin/bash

fw_depends java

gradle/wrapper

nohup build/hexagon/bin/hexagon &
