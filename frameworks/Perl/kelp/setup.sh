#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.pl
sed -i 's|user .*;|user '"$(id -u -n)"';|g' nginx.conf
sed -i 's|server unix.*frameworks-benchmark.sock;|server unix:'"${TROOT}"'/frameworks-benchmark.sock;|g' nginx.conf

fw_depends perl nginx

cpanm --notest --no-man-page \
    Kelp@0.9071 \
    DBI@1.636 \
    DBD::mysql@4.033 \
    MongoDB@1.4.2 \
    Kelp::Module::JSON::XS@0.502 \
    HTML::Escape@1.10 \
    HTTP::Parser::XS@0.17 \
    Starman@0.4014

nginx -c ${TROOT}/nginx.conf

plackup -E production -s Starman --workers=${MAX_THREADS} -l ${TROOT}/frameworks-benchmark.sock -a ./app.pl &
