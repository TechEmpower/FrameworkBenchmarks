#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' config.json

fw_depends java8 hot

${HOT_HOME}/hot run
