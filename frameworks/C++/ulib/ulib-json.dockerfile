FROM buildpack-deps:xenial

RUN apt update -yqq
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y && apt update -yqq
RUN apt install -yqq software-properties-common unzip cmake > /dev/null
RUN apt install -yqq libmysqlclient-dev libpq-dev > /dev/null
RUN apt install -yqq gcc-6 g++-6 > /dev/null
RUN apt install -yqq postgresql-server-dev-all libcap2-bin > /dev/null

ENV IROOT=/install
ENV ULIB_VERSION=1.4.2
ENV ULIB_ROOT=$IROOT/ULib
ENV ULIB_DOCUMENT_ROOT=$ULIB_ROOT/ULIB_DOCUMENT_ROOT
ENV CC=gcc-6
ENV CXX=g++-6
ENV AR=gcc-ar-6
ENV RANLIB=gcc-ranlib-6

RUN mkdir -p $ULIB_ROOT
RUN mkdir -p $ULIB_DOCUMENT_ROOT

WORKDIR $IROOT

# We need to install mongo-c-driver (we don't have a ubuntu package)
RUN wget -q https://github.com/mongodb/mongo-c-driver/releases/download/1.1.10/mongo-c-driver-1.1.10.tar.gz
RUN tar -xzf mongo-c-driver-1.1.10.tar.gz
RUN cd mongo-c-driver-1.1.10/ && \
    ./configure --prefix=$IROOT --libdir=$IROOT && \
    make && make install

# 1. Download ULib
RUN wget -q -O ULib-${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz
RUN tar xf ULib-${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)
# AVOID "configure: error: newly created file is older than distributed files! Check your system clock"
WORKDIR $IROOT/ULib-$ULIB_VERSION
RUN find . -exec touch {} \;
RUN cp -r tests/examples/benchmark/FrameworkBenchmarks/ULib/db $ULIB_ROOT

RUN echo "userver {" >> $ULIB_ROOT/benchmark.cfg
RUN echo "PORT 8080" >> $ULIB_ROOT/benchmark.cfg
RUN echo "PREFORK_CHILD 2" >> $ULIB_ROOT/benchmark.cfg
RUN echo "TCP_LINGER_SET 0 " >> $ULIB_ROOT/benchmark.cfg
RUN echo "LISTEN_BACKLOG 16384" >> $ULIB_ROOT/benchmark.cfg
RUN echo "ORM_DRIVER \"mysql pgsql\"" >> $ULIB_ROOT/benchmark.cfg
RUN echo "CLIENT_FOR_PARALLELIZATION 100" >> $ULIB_ROOT/benchmark.cfg
RUN echo "DOCUMENT_ROOT $ULIB_DOCUMENT_ROOT " >> $ULIB_ROOT/benchmark.cfg
RUN echo "}" >> $ULIB_ROOT/benchmark.cfg

RUN USP_FLAGS="-DAS_cpoll_cppsp_DO" \
    ./configure --prefix=$ULIB_ROOT \
    --disable-static --disable-examples \
    --with-mysql --with-pgsql \
    --without-ssl --without-pcre --without-expat \
    --without-libz --without-libuuid --without-magic --without-libares \
    --enable-static-orm-driver='mysql pgsql' --enable-static-server-plugin=http \
    --with-mongodb --with-mongodb-includes="-I$IROOT/include/libbson-1.0 -I$IROOT/include/libmongoc-1.0" --with-mongodb-ldflags="-L$IROOT"
# --enable-debug \
#USP_LIBS="-ljson" \
#cp $TROOT/src/* src/ulib/net/server/plugin/usp

RUN make install
RUN cd examples/userver && make install

# Compile usp pages (no more REDIS)
RUN cd src/ulib/net/server/plugin/usp && \
    make json.la plaintext.la db.la query.la update.la fortune.la cached_worlds.la \
          mdb.la mquery.la mupdate.la mfortune.la && \
    cp .libs/json.so .libs/plaintext.so \
        .libs/db.so   .libs/query.so  .libs/update.so  .libs/fortune.so .libs/cached_worlds.so \
        .libs/mdb.so  .libs/mquery.so .libs/mupdate.so .libs/mfortune.so $ULIB_DOCUMENT_ROOT

ENV PATH=${ULIB_ROOT}/bin:${PATH}

ADD ./ /ulib
WORKDIR /ulib


# 1. Change ULib Server (userver_tcp) configuration
RUN sed -i "s|TCP_LINGER_SET .*|TCP_LINGER_SET 0|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 256|g" $IROOT/ULib/benchmark.cfg
RUN sed -i "s|CLIENT_FOR_PARALLELIZATION .*|CLIENT_FOR_PARALLELIZATION 100|g"  $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
ENV UMEMPOOL="58,0,0,41,273,-15,-14,-20,36"

CMD sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD $(( 3 * $(nproc) / 2 ))|g"  $IROOT/ULib/benchmark.cfg && \
    setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip  $IROOT/ULib/bin/userver_tcp && \
    $IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg
