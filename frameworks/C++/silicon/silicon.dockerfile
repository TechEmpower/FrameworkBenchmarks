FROM buildpack-deps:xenial

RUN apt-get update -yqq && apt-get install -yqq software-properties-common cmake apt-transport-https

RUN add-apt-repository -s "deb http://apt.llvm.org/`lsb_release -cs`/ llvm-toolchain-`lsb_release -cs`-3.9 main"
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN apt-get update -yqq
RUN apt-get install -yqq clang-3.9 lldb-3.9

ENV MICROHTTPD_VERSION=0.9.39
ENV MICROHTTPD=/libmicrohttpd
ENV MICROHTTPD_HOME=$MICROHTTPD-$VERSION

RUN wget http://mirror.ibcp.fr/pub/gnu/libmicrohttpd/libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN tar xf libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN cd libmicrohttpd-$MICROHTTPD_VERSION && \
    ./configure --prefix=$MICROHTTPD_HOME && \
    make install

ENV PATH=${MICROHTTPD_HOME}/bin:${PATH}

RUN apt-get install -yqq libboost-dev cmake

ENV SILICON=/silicon

COPY ./ ./

RUN git clone https://github.com/matt-42/silicon.git && \
    cd silicon && \
    git checkout ecaf04887c9dbbf0f457afab1f487268f6aeffab && \
    CC=clang-3.9 CXX=clang++-3.9 ./install.sh /

RUN cd build && \
    cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9 && \
    make silicon_tpc_mysql

EXPOSE 8080

CMD /build/silicon_tpc_mysql tfb-database 8080 $(nproc)
