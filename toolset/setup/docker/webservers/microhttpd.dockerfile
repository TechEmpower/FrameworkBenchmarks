FROM tfb/clang-3.9:latest

ENV MICROHTTPD_VERSION=0.9.39
ENV MICROHTTPD=/libmicrohttpd
ENV MICROHTTPD_HOME=$MICROHTTPD-$VERSION

RUN wget http://mirror.ibcp.fr/pub/gnu/libmicrohttpd/libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN tar xf libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN cd libmicrohttpd-$MICROHTTPD_VERSION && \
    ./configure --prefix=$MICROHTTPD_HOME && \
    make install

ENV PATH=${MICROHTTPD_HOME}/bin:${PATH}
