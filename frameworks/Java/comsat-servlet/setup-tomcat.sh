#!/bin/bash

PROPS="-DserverClass=co.paralleluniverse.embedded.containers.TomcatServer -Dhello.TomcatServer.defaultResDir=."

. $TROOT/setup-generic.sh
