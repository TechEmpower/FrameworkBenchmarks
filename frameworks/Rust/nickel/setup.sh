#!/bin/bash

source $IROOT/rust.installed

rm -rf target/
cargo build --release
./target/release/nickel &
