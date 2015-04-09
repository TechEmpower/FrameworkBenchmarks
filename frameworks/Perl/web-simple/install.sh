#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18

fw_depends perl nginx

${PERL_HOME}/bin/cpanm --notest --no-man-page Web::Simple DBI DBD::mysql Plack Starman JSON::XS
echo installed Web::Simple app dependencies