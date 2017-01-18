#!/bin/bash

fw_depends postgresql lua luarocks openresty

luarocks install lapis
lapis server production &
