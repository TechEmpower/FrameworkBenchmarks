#!/bin/bash

VERSION="5.18"
PERL=$IROOT/perl-5.18
RETCODE=$(fw_exists ${PERL}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $PERL.installed
  return 0; }

fw_get https://raw.github.com/tokuhirom/Perl-Build/master/perl-build -o perl-build.pl
# compile with optimizations, n.b. this does not turn on debugging
perl perl-build.pl -DDEBUGGING=-g 5.18.2 perl-$VERSION 2>&1 | tee $IROOT/perl-install.log | awk '{ if (NR%100 == 0) printf "."}'

fw_get http://cpanmin.us -o cpanminus.pl
perl-$VERSION/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
# Install only a bare-bones of Perl modules
# Install others in the per-framework install script or cpanfile
perl-$VERSION/bin/cpanm -f --notest --no-man-page Carton JSON JSON::XS IO::Socket::IP IO::Socket::SSL

echo "export PERL_HOME=${PERL}" > $PERL.installed
echo -e "export PATH=:${PERL}/bin:\$PATH" >> $PERL.installed

source $PERL.installed
