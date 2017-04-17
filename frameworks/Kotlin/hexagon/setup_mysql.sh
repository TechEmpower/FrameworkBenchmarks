#!/bin/bash

fw_depends mysql java

gradle/wrapper

nohup build/install/hexagon/bin/hexagon mysql &
