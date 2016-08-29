#!/bin/bash

fw_depends java scala sbt

sbt stage

./target/universal/stage/bin/s-server-tfb -J-XX:+UseBiasedLocking -J-XX:+UseParallelGC -J-XX:+AggressiveOpts &