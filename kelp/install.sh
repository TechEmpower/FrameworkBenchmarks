#!/bin/bash

fw_depends perl

perl-5.18/bin/cpanm --notest --no-man-page Kelp DBI DBD::mysql Kelp::Module::JSON::XS Plack Starman
echo installed Kelp app dependencies
