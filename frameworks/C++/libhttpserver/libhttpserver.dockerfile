FROM buildpack-deps:xenial

RUN apt-get update -yqq && apt-get install -yqq software-properties-common unzip cmake

ENV LHT_HOME /libhttpserver

WORKDIR ${LHT_HOME}

ENV LIBMICROHTTPD_VERSION 0.9.59
ENV LIBHTTPSERVER_VERSION 0.17.5

RUN curl https://s3.amazonaws.com/libhttpserver/libmicrohttpd_releases/libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz -o libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz
RUN tar -xvzf libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz
RUN cd libmicrohttpd-${LIBMICROHTTPD_VERSION} && ./configure --disable-examples && make && make install
RUN curl -L https://github.com/etr/libhttpserver/archive/${LIBHTTPSERVER_VERSION}.tar.gz -o libhttpserver-${LIBHTTPSERVER_VERSION}.tar.gz
RUN tar -xvzf libhttpserver-${LIBHTTPSERVER_VERSION}.tar.gz
RUN cd libhttpserver-${LIBHTTPSERVER_VERSION} && ./bootstrap && mkdir build && cd build && ../configure --enable-fastopen && make && make install

COPY benchmark.cpp benchmark.cpp

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib

RUN g++ -O3 -std=c++11 -o benchmark benchmark.cpp -lhttpserver -L/usr/local/lib

EXPOSE 8080

CMD ./benchmark 8080 $(nproc)
