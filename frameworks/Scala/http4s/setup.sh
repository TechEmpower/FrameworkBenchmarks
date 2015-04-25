#!/bin/bash

source $IROOT/java8.installed
export SBT_HOME=${IROOT}/sbt

${SBT_HOME}/bin/sbt update compile
