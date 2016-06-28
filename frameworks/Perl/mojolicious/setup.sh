#!/bin/bash

fw_depends perl

carton install --cpanfile ${TROOT}/cpanfile

export LIBEV_FLAGS=7
HYPNOTOAD=$(${PERL_HOME}/bin/carton exec which hypnotoad)

carton exec perl ${HYPNOTOAD} ${TROOT}/app.pl
