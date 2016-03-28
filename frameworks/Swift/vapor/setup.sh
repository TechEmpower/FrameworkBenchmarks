#!/bin/bash

eval "$(curl -sL https://gist.githubusercontent.com/kylef/5c0475ff02b7c7671d2a/raw/02090c7ede5a637b76e6df1710e83cd0bbe7dcdf/swiftenv-install.sh)"

# git clone https://github.com/qutheory/vapor
# cd vapor
# sudo make install

# vapor build --release

swift build -c release -v
printenv
.build/release/App &


# .build/VaporApp &