#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.psgi
sed -i 's|server unix:.*/FrameworkBenchmarks/plack|server unix:'"${TROOT}"'|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page \
    JSON::XS@3.01 \
    HTTP::Parser::XS@0.16 \
    Plack@1.0030 \
    DBI@1.631 \
    DBD::mysql@4.033 \
    Starlet@0.24
    
nginx -c $TROOT/nginx.conf
start_server --backlog=16384 --pid-file=$TROOT/app.pid --path=$TROOT/app.sock -- plackup -E production -s Starlet --max-keepalive-reqs 1000 --max-reqs-per-child 50000 --min-reqs-per-child 40000 --max-workers=${MAX_THREADS} -a $TROOT/app.psgi &
