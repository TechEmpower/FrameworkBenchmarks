#!/bin/bash

fw_exists jester
[ $? -ne 0 ] || { return 0; }

git clone git://github.com/dom96/jester.git jester/jester