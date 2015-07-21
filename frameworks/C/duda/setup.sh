#!/bin/bash

export DUDA_HOME=${IROOT}/dudac-0.23
export PATH=${DUDA_HOME}:$PATH

dudac -w $TROOT/webservice -p 2001 &