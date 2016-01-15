#!/bin/bash

fw_depends java sbt

sbt update compile -batch

sbt -Ddb.host=${DBHOST} run &
