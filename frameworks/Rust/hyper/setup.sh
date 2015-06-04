#!/bin/bash

# Something like this will need to be done once we add database tests
# sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/main.rs

CARGO = $IROOT/rust/bin/cargo

$CARGO build --release

$CARGO run --release &
