#!/bin/bash

fw_depends postgresql rust

cargo clean
cargo build --release
cargo run --release &
