#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/perl-5.18.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get https://raw.github.com/tokuhirom/Perl-Build/master/perl-build -O perl-build.pl
# compile with optimizations, n.b. this does not turn on debugging
perl perl-build.pl -DDEBUGGING=-g 5.18.2 perl-5.18 2>&1 | tee $IROOT/perl-install.log | awk '{ if (NR%100 == 0) printf "."}'

fw_get http://cpanmin.us -O cpanminus.pl
perl-5.18/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
# Install only a bare-bones of Perl modules
# Install others in the per-framework install script or cpanfile
perl-5.18/bin/cpanm -f --notest --no-man-page Carton JSON JSON::XS IO::Socket::IP IO::Socket::SSL

touch ${IROOT}/perl-5.18.installed
