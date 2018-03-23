#!/bin/bash

fw_depends postgresql rust

cargo clean
RUSTFLAGS="-C target-cpu=native" cargo build --release
./target/release/actix-pg &
