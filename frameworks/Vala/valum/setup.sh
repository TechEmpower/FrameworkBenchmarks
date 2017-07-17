#!/bin/bash

fw_depends meson vala valum libjson-glib-dev

rm -rf build
meson --buildtype=release build
ninja -C build

build/app --forks=$(nproc --ignore 1) &
