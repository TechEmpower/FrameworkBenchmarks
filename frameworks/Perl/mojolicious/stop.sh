#!/bin/bash

HYPNOTOAD=$(${PERL_HOME}/bin/carton exec which hypnotoad)
${PERL_HOME}/bin/carton exec ${PERL_HOME}/bin/perl ${HYPNOTOAD} -s ${TROOT}/app.pl

