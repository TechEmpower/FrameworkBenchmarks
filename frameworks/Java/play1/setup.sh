#!/bin/bash
export PLAY1_HOME=${IROOT}/play-1.2.5

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

$PLAY1_HOME/play start --%prod