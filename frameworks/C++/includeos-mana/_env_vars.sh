if [ -z "$IROOT" ]; then
  IROOT=$HOME/FrameworkBenchmarks/installs
fi

echo "IROOT=$IROOT"
echo "TROOT=$TROOT"
export INCLUDEOS_PREFIX=$IROOT/IncludeOS_install
echo "INCLUDEOS_PREFIX=$INCLUDEOS_PREFIX"

export PATH=$PATH:$INCLUDEOS_PREFIX/bin

export CC=/usr/bin/clang-3.8
export CXX=/usr/bin/clang++-3.8
