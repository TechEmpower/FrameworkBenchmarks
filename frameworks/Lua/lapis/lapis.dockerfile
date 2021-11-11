FROM buildpack-deps:xenial

ENV LUA_VERSION="5.1"
ENV LUA_MICRO="5"

RUN apt-get update -yqq && apt-get install -yqq unzip

RUN wget https://github.com/LuaDist/lua/archive/$LUA_VERSION.$LUA_MICRO.tar.gz
RUN tar xf $LUA_VERSION.$LUA_MICRO.tar.gz

ENV LUA_HOME=/lua-$LUA_VERSION.$LUA_MICRO

RUN cd $LUA_HOME && \
    cp src/luaconf.h.orig src/luaconf.h && \
    make linux && \
    cd src && \
    mkdir ../bin ../include ../lib && \
    install -p -m 0755 lua luac ../bin && \
    install -p -m 0644 lua.h luaconf.h lualib.h lauxlib.h ../include && \
    install -p -m 0644 liblua.a ../lib

ENV LUA=/lua${LUA_VERSION}.${LUA_MICRO}
ENV PATH=${LUA_HOME}/bin:${PATH}
ENV LUA_PATH="./?.lua;./?.lc;$LUA_HOME/share/lua/5.1/?/init.lua;$LUA_HOME/share/lua/5.1/?.lua;$LUA_HOME/lib/lua/5.1/?/init.lua;$LUA_HOME/lib/lua/5.1/?.lua"
ENV LUA_CPATH="./?.lua;./?.lc;$LUA_HOME/share/lua/5.1/?/init.so;$LUA_HOME/share/lua/5.1/?.so;$LUA_HOME/lib/lua/5.1/?/init.so;$LUA_HOME/lib/lua/5.1/?.so"
ENV LUAROCKS_VERSION="2.2.1"
ENV LUAROCKS=/luarocks-$LUAROCKS_VERSION

RUN wget -q http://luarocks.org/releases/luarocks-$LUAROCKS_VERSION.tar.gz
RUN tar xf luarocks-$LUAROCKS_VERSION.tar.gz

RUN cd $LUAROCKS && \
    ./configure --prefix=$LUA_HOME --with-lua=$LUA_HOME && \
    make --quiet bootstrap

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

ADD ./ /lapis
WORKDIR /lapis

RUN luarocks install lua-resty-template
RUN luarocks install lapis

EXPOSE 8080

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|DBHOSTNAME|$DBIP|g" nginx.conf && \
    sed -i "s|DBHOSTNAME|$DBIP|g" config.moon && \
    sed -i "s|DBHOSTNAME|$DBIP|g" config.lua && \
    lapis server production
