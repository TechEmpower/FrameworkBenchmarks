#!/bin/bash

fw_depends perl

carton install --cpanfile $TROOT/cpanfile
echo installed Mojolicious app dependencies
