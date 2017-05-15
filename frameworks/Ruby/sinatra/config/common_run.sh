#!/bin/bash

if [[ $LOGDIR == *postgres* ]] ; then
  DBTYPE=postgresql
else
  DBTYPE=mysql
fi

export DBTYPE

if [[ $LOGDIR == *jruby* ]] ; then
  . $(dirname $0)/config/java_tune.sh
fi
