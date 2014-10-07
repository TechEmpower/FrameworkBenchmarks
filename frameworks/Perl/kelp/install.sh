#!/bin/bash

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
echo installed Kelp app dependencies
