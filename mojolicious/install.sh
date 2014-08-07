#!/bin/bash

fw_depends perl

# Ensure Mango and Mojolicious are installed
fw_get http://cpanmin.us -O cpanminus.pl
echo Got cpanm
perl-5.18/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
echo installed cpanm
perl-5.18/bin/cpanm --notest --no-man-page --installdeps ../mojolicious
echo installed Mojolicious app dependencies
