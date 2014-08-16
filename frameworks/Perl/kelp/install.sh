#!/bin/bash

fw_depends perl nginx

cpanm --notest --no-man-page Kelp DBI DBD::mysql Kelp::Module::JSON::XS Plack Starman
echo installed Kelp app dependencies
