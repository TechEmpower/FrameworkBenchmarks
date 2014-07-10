#!/bin/bash

RETCODE=$(fw_exists grails-2.4.2)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.4.2.zip
unzip -o grails-2.4.2.zip