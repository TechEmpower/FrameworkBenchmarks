#!/bin/bash

fw_depends lua nginx openresty

sudo luarocks install --server=http://rocks.moonscript.org lapis
