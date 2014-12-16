#!/bin/bash

${SBT_HOME}/bin/sbt update compile

${SBT_HOME}/bin/sbt -Ddb.host=${DBHOST} run &