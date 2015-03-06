#!/bin/bash

export ORM_DRIVER="sqlite"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=${ULIB_ROOT}/db/%.*s"

export UMEMPOOL="176,100,0,37,1110,105,-17,-22,40"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" ${IROOT}/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
${IROOT}/ULib/bin/userver_tcp -c ${IROOT}/ULib/benchmark.cfg &
