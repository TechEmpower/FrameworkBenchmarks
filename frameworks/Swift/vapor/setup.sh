#!/bin/bash

# install swiftenv for managing Swift versions
eval "$(curl -sL https://gist.githubusercontent.com/kylef/5c0475ff02b7c7671d2a/raw/02090c7ede5a637b76e6df1710e83cd0bbe7dcdf/swiftenv-install.sh)"

# install Swift compiler dependencies
sudo apt-get -y install clang libicu-dev

# compile the project with optimizations
swift build -c release

# run in the background
.build/release/App &

