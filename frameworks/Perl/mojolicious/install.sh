#!/bin/bash

fw_depends perl

${PERL_HOME}/bin/carton install --cpanfile $TROOT/cpanfile
echo installed Mojolicious app dependencies
