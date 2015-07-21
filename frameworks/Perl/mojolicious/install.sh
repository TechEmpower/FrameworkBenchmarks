#!/bin/bash
export PERL_HOME=${IROOT}/perl-5.18

fw_depends perl

${PERL_HOME}/bin/carton install --cpanfile ${TROOT}/cpanfile

