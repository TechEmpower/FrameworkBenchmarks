#!/bin/bash

fw_depends java7 sbt

sbt update compile

sbt -Ddb.host=${DBHOST} run &
