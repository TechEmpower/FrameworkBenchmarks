#!/bin/bash

export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64
export RESIN_HOME=${IROOT}/resin-4.0.41
export LEIN_HOME=$IROOT/bin

export PATH="$JAVA_HOME/bin:$LEIN_HOME:$PATH"