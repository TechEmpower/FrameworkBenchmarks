#!/bin/bash

eval "$(curl -sL https://gist.githubusercontent.com/kylef/5c0475ff02b7c7671d2a/raw/02090c7ede5a637b76e6df1710e83cd0bbe7dcdf/swiftenv-install.sh)"

swift build --fetch
rm -rf Packages/*/Tests
swift build -c release
.build/release/App &