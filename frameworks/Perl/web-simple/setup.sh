#!/bin/bash

sed -i 's|server unix:.*/FrameworkBenchmarks/web-simple|server unix:'"${TROOT}"'|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page  \
    Web::Simple@0.033 \
    DBI@1.637 \
    DBD::mysql@4.043 \
    Plack@1.0044 \
    Starman@0.4014 \
    JSON::XS@3.04
    
nginx -c $TROOT/nginx.conf

plackup -E production -s Starman --workers=${CPU_COUNT} -l $TROOT/frameworks-benchmark.sock -a $TROOT/app.pl &
