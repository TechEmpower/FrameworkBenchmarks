#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18
export NGINX_HOME=${IROOT}/nginx

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|server unix:.*/FrameworkBenchmarks/web-simple|server unix:'"${TROOT}"'|g' nginx.conf

export PATH="$PERL_HOME/bin:$PATH"

plackup -E production -s Starman --workers=${MAX_THREADS} -l $TROOT/frameworks-benchmark.sock -a $TROOT/app.pl &
$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf