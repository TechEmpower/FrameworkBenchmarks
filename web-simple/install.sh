#!/bin/bash

fw_depends perl

perl-5.18/bin/cpanm --notest --no-man-page Web::Simple DBI DBD::mysql Plack Starman JSON::XS
echo installed Web::Simple app dependencies
