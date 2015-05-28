#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

./gradlew clean --daemon

./gradlew run
