#!/bin/bash

fw_depends libreactor

[ "$TRAVIS" == "true" ] || sudo setcap cap_sys_nice,cap_sys_resource+eip $IROOT/libreactor_techempower/rest_server

rest_server &
