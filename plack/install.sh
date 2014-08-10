#!/bin/bash

fw_depends perl

cpanm --notest --no-man-page --installdeps $TROOT
echo installed Plack app dependencies
