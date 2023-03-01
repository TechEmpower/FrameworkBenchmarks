#!/bin/bash

# Update mORMot and static folder content from the latest [pre]release of mORMot2
# Required tools: jq wget 7zip. On Ubuntu can be installed by
# sudo apt install wget jq p7zip-full

# On error
err_report() {
  >&2 echo "Error in $0 on line $1"
  script_aborted
}
trap 'err_report $LINENO' ERR

script_successful(){
  echo "++Build successfully++"
  exit 0
}

script_aborted() {
  echo "******Build aborted******"
  exit 1
}

set -o pipefail

rm -rf ./libs

# echo "Getting the latest pre-release URL..."
# USED_TAG=$(wget -qO- https://api.github.com/repos/synopse/mORMot2/releases/latest | jq -r '.tag_name')
USED_TAG="2.0.4383"

echo "Used release tag $USED_TAG"
URL="https://github.com/synopse/mORMot2/releases/download/$USED_TAG/mormot2static.7z"
echo "Download statics from $URL ..."
wget -q -O./mormot2static.7z "$URL"

mkdir -p ./libs/mORMot/static
echo "Unpacking to ./libs/mORMot/static ..."
7za x ./mormot2static.7z -o./libs/mORMot/static
rm -rf ./mormot2static.7z

# uncomment for fixed commit URL
URL=https://github.com/synopse/mORMot2/tarball/9a186358a3695afc3065853790e90f2868d86e23
#URL="https://api.github.com/repos/synopse/mORMot2/tarball/$USED_TAG"
echo "Download and unpacking mORMot sources from $URL ..."
wget -qO- "$URL" | tar -xz -C ./libs/mORMot  --strip-components=1


# uncomment line below to echo commands to console
set -x

# get a mORMot folder name based on this script location
TARGET="${TARGET:-linux}"
ARCH="${ARCH:-x86_64}"
ARCH_TG="$ARCH-$TARGET"

MSRC="./libs/mORMot/src"
BIN="./bin"
STATIC="./libs/mORMot/static"

mkdir -p "$BIN/fpc-$ARCH_TG/.dcu"
rm -f "$BIN"/fpc-"$ARCH_TG"/.dcu/*

dest_fn=raw
if [[ $TARGET == win* ]]; then
  dest_fn="$dest_fn.exe"
fi

# suppress warnings
# Warning: (5059) Function result variable does not seem to be initialized
# Warning: (5036) Local variable XXX does not seem to be initialized
# Warning: (5089) Local variable XXX of a managed type does not seem to be initialized
# Warning: (5090) Variable XXX of a managed type does not seem to be initialized
SUPRESS_WARN=-vm11047,6058,5092,5091,5060,5058,5057,5028,5024,5023,4081,4079,4055,3187,3124,3123,5059,5036,5089,5090

echo "Start compiling..."
fpc -MDelphi -Sci -Ci -O4 -g -gl -gw2 -Xg -k'-rpath=$ORIGIN' -k-L$BIN \
  -T$TARGET -P$ARCH \
  -veiq -v-n-h- $SUPRESS_WARN \
  -Fi"$BIN/fpc-$ARCH_TG/.dcu" -Fi"$MSRC" \
  -Fl"$STATIC/$ARCH-$TARGET" \
  -Fu"$MSRC/core" -Fu"$MSRC/db" -Fu"$MSRC/rest" -Fu"$MSRC/crypt" \
    -Fu"$MSRC/app" -Fu"$MSRC/net" -Fu"$MSRC/lib" -Fu"$MSRC/orm" -Fu"$MSRC/soa" \
  -FU"$BIN/fpc-$ARCH_TG/.dcu" -FE"$BIN/fpc-$ARCH_TG" -o"$BIN/fpc-$ARCH_TG/$dest_fn" \
  -dFPC_LIBCMM -dNOSYNDBZEOS -dNOSYNDBIBX \
  -B -Se1 "./src/raw.pas" | grep "[Warning|Error|Fatal]:"

script_successful