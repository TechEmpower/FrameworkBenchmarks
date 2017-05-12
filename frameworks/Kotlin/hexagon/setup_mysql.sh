#!/bin/bash

fw_depends mysql java

gradle/wrapper -x test

nohup build/install/hexagon/bin/hexagon mysql &
