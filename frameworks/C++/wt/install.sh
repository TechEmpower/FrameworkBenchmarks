#!/bin/bash

export BOOST_ROOT=/usr/local
export BOOST_INC=${BOOST_ROOT}/include
export BOOST_LIB=${BOOST_ROOT}/lib

export WT_ROOT=${IROOT}/wt
export WT_LIB=${WT_ROOT}/lib
export WT_INC=${WT_ROOT}/include

export LD_LIBRARY_PATH="${BOOST_LIB}:${WT_LIB}:${LD_LIBRARY_PATH}"

fw_depends apache wt
