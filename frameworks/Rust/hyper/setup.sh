#!/bin/bash

fw_depends rust

cargo clean
cargo build --release
cargo run --release &
