#!/bin/bash

fw_depends ulib

# Travis is broken
if [ "$TRAVIS" != "true" ]; then
MAX_THREADS=$CPU_COUNT
else
MAX_THREADS=$(( 2 * $CPU_COUNT ))
fi

# 1. Change ULib Server (userver_tcp) configuration
sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g"										$IROOT/ULib/benchmark.cfg
sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 16384|g"								$IROOT/ULib/benchmark.cfg
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g"						$IROOT/ULib/benchmark.cfg
sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 8000|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
export UMEMPOOL="58,0,0,41,16401,-14,-15,11,25"

# Never use setcap inside of TRAVIS 
[ "$TRAVIS" != "true" ] || { \
if [ `ulimit -r` -eq 99 ]; then
	sudo setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip $IROOT/ULib/bin/userver_tcp
fi
}

$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
