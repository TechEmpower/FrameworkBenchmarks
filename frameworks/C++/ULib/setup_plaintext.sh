#!/bin/bash

# 1. Change ULib Server (userver_tcp) configuration
sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 16384|g"		  $IROOT/ULib/benchmark.cfg
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
export UMEMPOOL="50,0,0,0,8356,170,-13,-22,0"

$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
