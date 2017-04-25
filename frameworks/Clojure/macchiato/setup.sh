#!/bin/bash

fw_depends postgresql nodej java leiningen

# Update db host in the source file
sed -i 's|localhost|'"${DBHOST}"'|g' hello/src/hello/db.cljs

cd hello
lein package
node target/release/hello.js &
