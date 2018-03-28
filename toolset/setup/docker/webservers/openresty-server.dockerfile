FROM tfb/luarocks:latest

RUN apt install -yqq libpq-dev libpcre3 libpcre3-dev

ENV OPENRESTY_VERSION="1.11.2.1"
ENV OPENRESTY=/openresty
ENV OPENRESTY_HOME=$OPENRESTY-$OPENRESTY_VERSION

RUN wget http://openresty.org/download/openresty-$OPENRESTY_VERSION.tar.gz
RUN tar xf openresty-$OPENRESTY_VERSION.tar.gz

RUN cd openresty-$OPENRESTY_VERSION && \
    ./configure --with-http_postgres_module --prefix=$OPENRESTY_HOME --with-luajit-xcflags="-DLUAJIT_NUMMODE=2 -O3" --with-cc-opt="-O3" -j4 && \
    make -j4 --quiet && \
    make --quiet install

ENV OPENRESTY_HOME=${OPENRESTY_HOME}
ENV PATH=${OPENRESTY_HOME}/nginx/sbin:${PATH}
