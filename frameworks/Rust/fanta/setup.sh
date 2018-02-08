#!/bin/bash

fw_depends postgresql rust

cd ExampleApp;
cargo clean;
RUSTFLAGS="-C target-cpu=native" cargo build --release;
./target/release/ExampleApp &
