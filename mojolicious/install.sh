#!/bin/bash

fw_depends perl

perl-5.18/bin/cpanm --notest --no-man-page --installdeps ../mojolicious
echo installed Mojolicious app dependencies
