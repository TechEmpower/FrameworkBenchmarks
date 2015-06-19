#!/bin/bash

fw_depends ulib

# 1. Change ULib Server (userver_tcp) configuration
sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g"										$IROOT/ULib/benchmark.cfg
sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 16384|g"								$IROOT/ULib/benchmark.cfg
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g"						$IROOT/ULib/benchmark.cfg
sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 8000|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
export UMEMPOOL="982,0,0,36,9846,-24,-23,1727,1151"

$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
