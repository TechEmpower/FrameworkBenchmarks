. ../toolset/setup/linux/bash_functions.sh

fw_exists dart-sdk
[ $? -ne 0 ] || { echo "Dart is installed!"; return 0; }

fw_get http://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip
fw_unzip dartsdk-linux-x64-release.zip
