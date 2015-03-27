#!/bin/bash

export LIBEV_FLAGS=7
HYPNOTOAD=$(${PERL_HOME}/bin/carton exec which hypnotoad)
${PERL_HOME}/bin/carton exec ${PERL_HOME}/bin/perl ${HYPNOTOAD} ${TROOT}/app.pl
