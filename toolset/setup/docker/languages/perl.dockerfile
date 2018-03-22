FROM tfb/base:latest

ENV PERL_VERSION="5.18"
ENV PERL=/perl-${PERL_VERSION}

RUN wget -q -O perl-build.pl https://raw.github.com/tokuhirom/Perl-Build/master/perl-build
# compile with optimizations, n.b. this does not turn on debugging
RUN perl perl-build.pl -DDEBUGGING=-g 5.18.2 perl-${PERL_VERSION} 2>&1 | tee /perl-install.log | awk '{ if (NR%100 == 0) printf "."}'

RUN wget -O cpanminus.pl http://cpanmin.us
RUN perl-${PERL_VERSION}/bin/perl cpanminus.pl --notest --no-man-page App::cpanminus
# Install only a bare-bones of Perl modules
# Install others in the per-framework install script or cpanfile
RUN perl-${PERL_VERSION}/bin/cpanm -f --notest --no-man-page Carton JSON JSON::XS IO::Socket::IP IO::Socket::SSL

ENV PERL_HOME=${PERL}

#ENV PERL_CARTON_PATH=$TROOT/local
#ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
#ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}
