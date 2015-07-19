#!/bin/bash

PROPS="-DserverClass=co.paralleluniverse.embedded.containers.TomcatServer -Dco.paralleluniverse.embedded.containers.TomcatServer.defaultResDir=."

. $TROOT/setup-generic.sh
