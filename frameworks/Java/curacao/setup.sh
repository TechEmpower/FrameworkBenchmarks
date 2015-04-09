#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

${IROOT}/sbt/bin/sbt assembly

java -jar dist/curacao-standalone.jar &