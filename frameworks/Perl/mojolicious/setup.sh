#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18
export NGINX_HOME=${IROOT}/nginx

export LIBEV_FLAGS=7
HYPNOTOAD=$(${PERL_HOME}/bin/carton exec which hypnotoad)
${PERL_HOME}/bin/carton exec ${PERL_HOME}/bin/perl ${HYPNOTOAD} ${TROOT}/app.pl
