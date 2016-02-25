#!/bin/bash

fw_depends rust

cargo clean
cargo build --release
./target/release/nickel &
