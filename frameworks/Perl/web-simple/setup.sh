#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|server unix:.*/FrameworkBenchmarks/web-simple|server unix:'"${TROOT}"'|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page  \
    Web::Simple@0.031 \
    DBI@1.636 \
    DBD::mysql@4.033 \
    Plack@1.0039 \
    Starman@0.4014 \
    JSON::XS@3.02
    
nginx -c $TROOT/nginx.conf

plackup -E production -s Starman --workers=${MAX_THREADS} -l $TROOT/frameworks-benchmark.sock -a $TROOT/app.pl &
