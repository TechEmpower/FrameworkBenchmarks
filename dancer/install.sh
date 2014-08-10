#!/bin/bash

fw_depends perl

perl-5.18/bin/cpanm --notest --no-man-page Dancer Dancer::Plugin::Database DBI DBD::mysql JSON::XS Plack Starman
echo installed Dancer app dependencies
