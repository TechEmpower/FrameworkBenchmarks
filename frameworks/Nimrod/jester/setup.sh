#!/bin/bash
export JESTER_PATH=${IROOT}/jester
export NIMROD_HOME=${IROOT}/nimrod
export NGINX_HOME=${IROOT}/nginx

${NIMROD_HOME}/bin/nim c -d:release --path:$JESTER_PATH hello.nim
${NGINX_HOME}/sbin/nginx -c $TROOT/config/nginx.conf

current=9000
end=9008
while [ $current -lt $end ]; do
  ./hello $current &
  let current=current+1
done