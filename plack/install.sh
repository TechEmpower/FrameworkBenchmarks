#!/bin/bash

fw_depends perl

perl-5.18/bin/cpanm --notest --no-man-page --installdeps ../plack
echo installed Plack app dependencies
