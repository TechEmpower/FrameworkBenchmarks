#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18
export NGINX_HOME=${IROOT}/nginx

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
#sed -i 's|user .*;|user '"$(id -u -n)"';|g' nginx.conf
sed -i 's|server unix.*frameworks-benchmark.sock;|server unix:'"${TROOT}"'/frameworks-benchmark.sock;|g' nginx.conf

$NGINX_HOME/sbin/nginx -c ${TROOT}/nginx.conf

$PERL_HOME/bin/plackup -E production -s Starman --workers=${MAX_THREADS} -l ${TROOT}/frameworks-benchmark.sock -a ./app.pl &