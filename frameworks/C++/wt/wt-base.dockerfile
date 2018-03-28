FROM tfb/gcc-6:latest

ENV WT_VERSION=4.0.2
ENV BOOST_ROOT=${IROOT}/boost
ENV BOOST_INC=${BOOST_ROOT}/include
ENV BOOST_LIB=${BOOST_ROOT}/lib
ENV WT_ROOT=${IROOT}/wt
ENV WT_LIB=${WT_ROOT}/lib
ENV WT_INC=${WT_ROOT}/include
ENV LD_LIBRARY_PATH="${BOOST_LIB}:${WT_LIB}:${LD_LIBRARY_PATH}"
ENV CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.3/server:$CPLUS_INCLUDE_PATH

# Install CMake 3.x
RUN apt-add-repository --yes ppa:george-edison55/cmake-3.x
RUN apt-get update -qq
RUN apt-get install -qqy cmake

# Build boost_thread, boost_system, boost_filesystem and boost_program_options
RUN wget -q https://dl.bintray.com/boostorg/release/1.65.1/source/boost_1_65_1.tar.gz
RUN tar xf boost_1_65_1.tar.gz
RUN cd boost_1_65_1 && \
    ./bootstrap.sh && \
    ./b2 \
      -d0 \
      toolset=gcc-6 \
      variant=release \
      link=static \
      cxxflags="-std=c++14 -march=native" \
      cflags="-march=native" \
      --prefix=${BOOST_ROOT} \
      --with-system \
      --with-thread \
      --with-program_options \
      --with-filesystem \
      install

RUN wget -q https://github.com/emweb/wt/archive/$WT_VERSION.tar.gz
RUN mv $WT_VERSION.tar.gz wt-$WT_VERSION.tar.gz
RUN tar xf wt-$WT_VERSION.tar.gz

RUN cd wt-$WT_VERSION && \
    mkdir -p build && \
    cd build && \
    cmake .. -DCMAKE_CXX_STANDARD=14 -DCMAKE_BUILD_TYPE=Release \
      -DBOOST_PREFIX=${BOOST_ROOT} \
      -DCMAKE_INSTALL_PREFIX=${IROOT}/wt -DCONFIGDIR=${IROOT}/wt/etc \
      -DCMAKE_C_COMPILER=$(which gcc-6) \
      -DCMAKE_CXX_COMPILER=$(which g++-6) -DDESTDIR=${IROOT}/wt \
      -DWEBUSER=$(id -u -n) -DWEBGROUP=$(id -g -n) \
      -DENABLE_SSL=OFF -DHTTP_WITH_ZLIB=OFF \
      -DCMAKE_C_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
      -DCMAKE_CXX_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
      -DBUILD_TESTS=OFF -DENABLE_LIBWTTEST=OFF \
      -DSHARED_LIBS=OFF && \
    make && make install
