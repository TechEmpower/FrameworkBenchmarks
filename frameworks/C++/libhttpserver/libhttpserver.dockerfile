FROM buildpack-deps:xenial

RUN apt-get update -yqq > /dev/null && apt-get install -yqq software-properties-common unzip cmake > /dev/null

ENV LHT_HOME /libhttpserver

WORKDIR ${LHT_HOME}

ENV LIBMICROHTTPD_VERSION 0.9.59
ENV LIBHTTPSERVER_VERSION 0.17.5

RUN curl https://s3.amazonaws.com/libhttpserver/libmicrohttpd_releases/libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz -o libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz > /dev/null
RUN tar -xvzf libmicrohttpd-${LIBMICROHTTPD_VERSION}.tar.gz  > /dev/null
RUN cd libmicrohttpd-${LIBMICROHTTPD_VERSION} && ./configure --disable-examples && make --quiet && make install --quiet
RUN curl -L https://github.com/etr/libhttpserver/archive/${LIBHTTPSERVER_VERSION}.tar.gz -o libhttpserver-${LIBHTTPSERVER_VERSION}.tar.gz > /dev/null
RUN tar -xvzf libhttpserver-${LIBHTTPSERVER_VERSION}.tar.gz > /dev/null
RUN cd libhttpserver-${LIBHTTPSERVER_VERSION} && ./bootstrap && mkdir build && cd build && ../configure --enable-fastopen && make --quiet && make install --quiet

COPY benchmark.cpp benchmark.cpp

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib

RUN g++ -O3 -std=c++11 -o benchmark benchmark.cpp -lhttpserver -L/usr/local/lib

EXPOSE 8080

CMD ./benchmark 8080 $(nproc)
