#!/bin/bash

fw_depends play1 java

# Precompile the application and template
# Must be executed in the project folder
cd $FWROOT
$IROOT/play-1.2.5/play1 deps  --%prod
$IROOT/play-1.2.5/play1 precompile  --%prod