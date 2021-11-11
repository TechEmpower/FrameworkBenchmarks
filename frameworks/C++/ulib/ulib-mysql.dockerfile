FROM ubuntu:18.04

COPY ./ ./

RUN apt-get update -yqq && \
	 apt-get install -yqq software-properties-common build-essential curl locales wget unzip git \
    libmysqlclient-dev libpq-dev \
    libpcre3 libpcre3-dev \
    libssl-dev libcurl4-openssl-dev \
    zlib1g-dev \
    libreadline6-dev \
    libbz2-dev \
    libxslt-dev libgdbm-dev ncurses-dev  \
    libffi-dev libtool bison libevent-dev \
    liborc-0.4-0 \
    libmcrypt-dev libicu-dev \
    re2c libnuma-dev \
	 postgresql-server-dev-all libcap2-bin && \
	 add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
	 apt-get update -yqq && \
	 apt-get install -yqq gcc-8 g++-8

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

ENV CC=gcc-8
ENV CXX=g++-8
ENV AR=gcc-ar-8
ENV RANLIB=gcc-ranlib-8
ENV IROOT=/install
ENV ULIB_ROOT=$IROOT/ULib
ENV ULIB_VERSION=2.4.2
ENV ULIB_DOCUMENT_ROOT=$ULIB_ROOT/ULIB_DOCUMENT_ROOT

WORKDIR $IROOT

RUN mkdir -p $ULIB_DOCUMENT_ROOT
RUN wget -q -O ULib-${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz
RUN tar xf ULib-${ULIB_VERSION}.tar.gz

WORKDIR $IROOT/ULib-$ULIB_VERSION

# AVOID "configure: error: newly created file is older than distributed files! Check your system clock"
RUN cp /src/* src/ulib/net/server/plugin/usp
RUN find . -exec touch {} \;

RUN echo "userver {" >> $ULIB_ROOT/benchmark.cfg
RUN echo "PORT 8080" >> $ULIB_ROOT/benchmark.cfg
RUN echo "PREFORK_CHILD $(nproc)" >> $ULIB_ROOT/benchmark.cfg
RUN echo "TCP_LINGER_SET -2" >> $ULIB_ROOT/benchmark.cfg
RUN echo "LISTEN_BACKLOG 256" >> $ULIB_ROOT/benchmark.cfg
RUN echo "ORM_DRIVER mysql" >> $ULIB_ROOT/benchmark.cfg
RUN echo "DOCUMENT_ROOT $ULIB_DOCUMENT_ROOT " >> $ULIB_ROOT/benchmark.cfg
RUN echo "}" >> $ULIB_ROOT/benchmark.cfg

RUN USP_FLAGS="-DAS_cpoll_cppsp_DO" \
    ./configure --prefix=$ULIB_ROOT \
    --with-mysql \
    --disable-static --disable-examples \
    --without-ssl --disable-HCRS --without-pcre --without-expat \
    --without-libz --without-libuuid --disable-HPRS --without-magic --without-libares \
    --enable-static-orm-driver='mysql' --enable-static-server-plugin=http

RUN make install && \
	 cd examples/userver && make install && \
	 cd ../../src/ulib/net/server/plugin/usp && \
    AM_LDFLAGS="-lFortune" make fortune.la && \
	 AM_LDFLAGS="-lWorld" make db.la query.la update.la cached_worlds.la && \
    cp .libs/db.so .libs/query.so .libs/update.so .libs/fortune.so .libs/cached_worlds.so $ULIB_DOCUMENT_ROOT

ENV PATH=${ULIB_ROOT}/bin:${PATH}

ADD ./ /ulib
WORKDIR /ulib

ENV ORM_DRIVER="mysql"
ENV ORM_OPTION="host=tfb-database user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=hello_world"
ENV UMEMPOOL="96,0,0,97,16417,-14,-20,-18,26"

EXPOSE 8080

CMD setcap cap_sys_nice,cap_sys_resource,cap_net_bind_service,cap_net_raw+eip $IROOT/ULib/bin/userver_tcp && \
    $IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg
