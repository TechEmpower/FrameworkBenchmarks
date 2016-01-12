#!/bin/bash

fw_depends java sbt

sbt update compile

sbt -Ddb.host=${DBHOST} run &
