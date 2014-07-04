#!/bin/bash

fw_exists nawak
[ $? -ne 0 ] || { return 0; }

git clone git://github.com/idlewan/nawak.git nawak/nawak