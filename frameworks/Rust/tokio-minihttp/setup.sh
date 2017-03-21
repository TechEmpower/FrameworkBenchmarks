#!/bin/bash

fw_depends postgresql rust

cargo clean
cargo build --release
./target/release/tokio-minihttp &
