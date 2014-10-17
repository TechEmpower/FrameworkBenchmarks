#!/bin/bash

sudo apt-get install -y postgresql-server-dev-9.3 libpq-dev

fw_get -O cppsp_0.2.3.tar.xz http://downloads.sourceforge.net/project/cpollcppsp/CPPSP%200.2%20%28testing%29/cppsp_0.2.3.tar.xz
fw_untar cppsp_0.2.3.tar.xz

mv cppsp_rel0.2.3/ cppsp_0.2.3