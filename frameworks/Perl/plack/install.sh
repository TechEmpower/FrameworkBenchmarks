#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18

fw_depends perl nginx

${PERL_HOME}/bin/cpanm --notest --no-man-page --installdeps $TROOT
echo installed Plack app dependencies
