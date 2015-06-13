#!/bin/bash

source $IROOT/rust.installed

cargo clean
cargo build --release
cargo run --release &
