#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|server unix:.*/FrameworkBenchmarks/web-simple|server unix:'"${TROOT}"'|g' nginx.conf

export PATH="$PERL_HOME/bin:$PATH"

cpanm --notest --no-man-page Web::Simple DBI DBD::mysql Plack Starman JSON::XS
echo installed Web::Simple app dependencies

plackup -E production -s Starman --workers=${MAX_THREADS} -l $TROOT/frameworks-benchmark.sock -a $TROOT/app.pl &
$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf