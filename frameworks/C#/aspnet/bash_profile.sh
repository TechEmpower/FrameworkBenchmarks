#!/bin/bash

export MONO_ROOT=${IROOT}/mono-3.6.0-install

# Needed to find Mono's shared libraries
export LD_LIBRARY_PATH="$MONO_ROOT/lib"

export NGINX_HOME=${IROOT}/nginx