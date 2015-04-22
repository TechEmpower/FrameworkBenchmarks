#!/bin/bash

source $IROOT/java7.installed
export SBT_HOME=${IROOT}/sbt

${SBT_HOME}/bin/sbt update compile
