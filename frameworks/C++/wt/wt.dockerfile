FROM buildpack-deps:xenial

RUN apt-get update -yqq && apt-get install -yqq software-properties-common unzip cmake

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN apt-get update -yqq
RUN apt-get install -yqq gcc-6 g++-6

ENV WT_VERSION 4.0.2
ENV BOOST_ROOT /boost
ENV BOOST_INC ${BOOST_ROOT}/include
ENV BOOST_LIB ${BOOST_ROOT}/lib
ENV WT_ROOT /wt
ENV WT_LIB ${WT_ROOT}/lib
ENV WT_INC ${WT_ROOT}/include
ENV LD_LIBRARY_PATH ${BOOST_LIB}:${WT_LIB}:${LD_LIBRARY_PATH}
ENV CPLUS_INCLUDE_PATH /usr/include/postgresql:/usr/include/postgresql/9.3/server:${CPLUS_INCLUDE_PATH}

WORKDIR ${WT_ROOT}
COPY benchmark.cpp benchmark.cpp
COPY fortunes.xml fortunes.xml
COPY wt_config.xml wt_config.xml

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

RUN wget -q https://github.com/emweb/wt/archive/${WT_VERSION}.tar.gz
RUN mv ${WT_VERSION}.tar.gz wt-${WT_VERSION}.tar.gz
RUN tar xf wt-${WT_VERSION}.tar.gz

RUN cd wt-$WT_VERSION && \
    mkdir -p build && \
    cd build && \
    cmake .. -DCMAKE_CXX_STANDARD=14 -DCMAKE_BUILD_TYPE=Release \
      -DBOOST_PREFIX=${BOOST_ROOT} \
      -DCMAKE_INSTALL_PREFIX=${WT_ROOT} -DCONFIGDIR=${WT_ROOT}/etc \
      -DCMAKE_C_COMPILER=$(which gcc-6) \
      -DCMAKE_CXX_COMPILER=$(which g++-6) -DDESTDIR=${WT_ROOT} \
      -DWEBUSER=$(id -u -n) -DWEBGROUP=$(id -g -n) \
      -DENABLE_SSL=OFF -DHTTP_WITH_ZLIB=OFF \
      -DCMAKE_C_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
      -DCMAKE_CXX_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
      -DBUILD_TESTS=OFF -DENABLE_LIBWTTEST=OFF \
      -DSHARED_LIBS=OFF >/dev/null && \
    make && make install

RUN g++-6 \
  -std=c++14 \
  -O3 -march=native -DNDEBUG \
  -I${BOOST_INC} \
  -L${BOOST_LIB} \
  -I${WT_INC} \
  -L${WT_LIB} \
  -o te-benchmark.wt \
  benchmark.cpp \
  -lwthttp -lwt \
  -lwtdbo -lwtdbomysql \
  -lboost_system \
  -lboost_program_options \
  -lboost_thread \
  -lboost_filesystem \
  -lpthread \
  -lmysqlclient

ENV DBHOST tfb-database

EXPOSE 8080

CMD ./te-benchmark.wt -c wt_config.xml -t $(nproc) --docroot . --approot . --http-listen 0.0.0.0:8080 --accesslog=- --no-compression
