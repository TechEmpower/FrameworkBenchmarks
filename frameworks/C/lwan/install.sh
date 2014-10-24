#!/bin/bash

REV='aa6c15fbdf63d9db722ddc72bd736b23381331be'

INSTALLED_FILE="${IROOT}/lwan-${REV}.installed"
RETCODE=$(fw_exists ${INSTALLED_FILE})
[ ! "$RETCODE" == 0 ] || { return 0; }

# Lwan is only built during installation as a dependency sanity check.
sudo apt-get update && \
	sudo apt-get install libjemalloc-dev && \
	git clone git://github.com/lpereira/lwan.git && \
        cd lwan && \
        git checkout ${REV} && \
        mkdir build && \
        cd build && \
        cmake .. -DCMAKE_BUILD_TYPE=Release && \
        make techempower && \
        touch ${INSTALLED_FILE}
