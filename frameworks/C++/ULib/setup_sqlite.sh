#!/bin/bash

export UMEMPOOL="152,85,0,37,8315,105,-17,-22,40"
export ORM_DRIVER="sqlite"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=${ULIB_ROOT}/db/%.*s"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" ${IROOT}/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
${IROOT}/ULib/bin/userver_tcp -c ${IROOT}/ULib/benchmark.cfg &
