#!/bin/bash

fw_depends perl nginx

${PERL_HOME}/bin/cpanm --notest --no-man-page Dancer Dancer::Plugin::Database DBI DBD::mysql JSON::XS Plack Starman
echo installed Dancer app dependencies
