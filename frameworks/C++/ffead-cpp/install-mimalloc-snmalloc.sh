apt install -y clang-format-9 ninja-build

wget -q https://github.com/microsoft/mimalloc/archive/v2.0.2.tar.gz
tar xf mimalloc-2.0.2.tar.gz
cd mimalloc-2.0.2
mkdir -p out/release
cmake ../.. -DCMAKE_BUILD_TYPE=Release
make && make install
cd $IROOT
rm -rf mimalloc-2.0.2

wget -q https://github.com/microsoft/snmalloc/archive/0.5.3.tar.gz
tar xf snmalloc-0.5.3.tar.gz
cd snmalloc-0.5.3
mkdir build
cmake -G Ninja .. -DCMAKE_BUILD_TYPE=Release
ninja
cd $IROOT
rm -rf snmalloc-0.5.3