#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/perl.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/perl.installed
  return 0; }

VERSION="5.18"
PERL=$IROOT/perl-$VERSION

fw_get -o perl-build.pl https://raw.github.com/tokuhirom/Perl-Build/master/perl-build
# compile with optimizations, n.b. this does not turn on debugging
perl perl-build.pl -DDEBUGGING=-g 5.18.2 perl-$VERSION 2>&1 | tee $IROOT/perl-install.log | awk '{ if (NR%100 == 0) printf "."}'

fw_get -o cpanminus.pl http://cpanmin.us
perl-$VERSION/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
# Install only a bare-bones of Perl modules
# Install others in the per-framework install script or cpanfile
perl-$VERSION/bin/cpanm -f --notest --no-man-page Carton JSON JSON::XS IO::Socket::IP IO::Socket::SSL

echo "export PERL_HOME=${PERL}" > $IROOT/perl.installed
echo -e "export PERL_CARTON_PATH=\$TROOT/local" >> $IROOT/perl.installed
echo -e "export PERL5LIB=\$PERL_CARTON_PATH/lib/perl5" >> $IROOT/perl.installed
echo -e "export PATH=\$PERL_CARTON_PATH/bin:\$PERL_HOME/bin:\$PATH" >> $IROOT/perl.installed

source $IROOT/perl.installed
