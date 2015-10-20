#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|user .*;|user '"$(id -u -n)"';|g' nginx.conf
sed -i 's|server unix.*frameworks-benchmark.sock;|server unix:'"${TROOT}"'/frameworks-benchmark.sock;|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page \
    Kelp \
    DBI \
    DBD::mysql \
    MongoDB \
    Kelp::Module::JSON::XS \
    HTML::Escape \
    HTTP::Parser::XS \
    Starman

nginx -c ${TROOT}/nginx.conf

plackup -E production -s Starman --workers=${MAX_THREADS} -l ${TROOT}/frameworks-benchmark.sock -a ./app.pl &
