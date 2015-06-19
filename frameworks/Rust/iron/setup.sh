#!/bin/bash

fw_depends rust

rm -rf target/
cargo build --release
./target/release/iron &
