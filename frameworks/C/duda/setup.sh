#!/bin/bash

export DUDA_HOME=${IROOT}/dudac
export PATH=${DUDA_HOME}:$PATH

dudac -w $TROOT/webservice -p 2001 &