#!/bin/bash

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

fw_depends java play1 

$PLAY1_HOME/play start --%prod
