#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists grails-2.4.2
[ $? -ne 0 ] || { echo "Grails is installed!"; return 0; }

fw_get http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.4.2.zip
unzip -o grails-2.4.2.zip