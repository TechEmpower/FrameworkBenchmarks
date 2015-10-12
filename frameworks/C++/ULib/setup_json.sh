#!/bin/bash

export UMEMPOOL="56,0,0,40,150,-24,-13,-20,0"

fw_depends ulib

# 1. Change ULib Server (userver_tcp) configuration
sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g"										$IROOT/ULib/benchmark.cfg
sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 256|g"								  $IROOT/ULib/benchmark.cfg
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g"					  $IROOT/ULib/benchmark.cfg
sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 100|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
userver_tcp -c $IROOT/ULib/benchmark.cfg &
