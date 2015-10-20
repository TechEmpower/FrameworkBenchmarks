#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|server unix:.*/FrameworkBenchmarks/web-simple|server unix:'"${TROOT}"'|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page Web::Simple DBI DBD::mysql Plack Starman JSON::XS

nginx -c $TROOT/nginx.conf

plackup -E production -s Starman --workers=${MAX_THREADS} -l $TROOT/frameworks-benchmark.sock -a $TROOT/app.pl &
