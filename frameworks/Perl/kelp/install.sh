#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18

fw_depends perl nginx

${PERL_HOME}/bin/cpanm --notest --no-man-page \
    Kelp \
    DBI \
    DBD::mysql \
    MongoDB \
    Kelp::Module::JSON::XS \
    HTML::Escape \
    HTTP::Parser::XS \
    Starman
echo installed Kelp app dependencies
