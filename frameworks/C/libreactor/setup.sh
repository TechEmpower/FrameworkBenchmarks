#!/bin/bash

fw_depends libreactor

make clean && make

./libreactor &
