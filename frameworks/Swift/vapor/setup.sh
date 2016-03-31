#!/bin/bash

eval "$(curl -sL https://gist.githubusercontent.com/kylef/5c0475ff02b7c7671d2a/raw/02090c7ede5a637b76e6df1710e83cd0bbe7dcdf/swiftenv-install.sh)"

# git clone https://github.com/qutheory/vapor
# cd vapor
# sudo make install

# vapor build --release

sudo apt-get install clang libicu-dev
# printenv
<<<<<<< HEAD
swift build -c release -v -Xlinker
.build/debug/App &
=======
# swift build -c release -v -Xlinker -v
export LD_LIBRARY_PATH=/home/travis/build/qutheory/FrameworkBenchmarks/frameworks/Swift/vapor/.build/release
.build/release/App &
>>>>>>> f3356432e4f275efabb4897d9d45b86fda8f2ae8


# .build/VaporApp &
