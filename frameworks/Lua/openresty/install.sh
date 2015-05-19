#!/bin/bash

fw_depends lua openresty

sudo luarocks install --server=http://rocks.moonscript.org lua-resty-template
