#!/bin/bash

fw_depends perl nginx

cpanm --notest --no-man-page \
    Dancer@1.3134 \
    Dancer::Plugin::Database@2.10 \
    DBI@1.633 \
    DBD::mysql@4.033 \
    JSON::XS@3.01 \
    Plack@1.0034 \
    Starman@0.4011
    
nginx -c ${TROOT}/nginx.conf

plackup -E production -s Starman --workers=${CPU_COUNT} -l /tmp/perl-dancer.sock -a ./app.pl &
