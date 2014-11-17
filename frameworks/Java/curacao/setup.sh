#!/bin/bash

${IROOT}/sbt/bin/sbt assembly

java -jar dist/curacao-standalone.jar &