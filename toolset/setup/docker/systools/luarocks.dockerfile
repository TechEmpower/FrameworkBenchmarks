FROM tfb/lua:latest

ENV LUAROCKS_VERSION="2.2.1"
ENV LUAROCKS=/luarocks-$LUAROCKS_VERSION

RUN wget http://luarocks.org/releases/luarocks-$LUAROCKS_VERSION.tar.gz
RUN tar xf luarocks-$LUAROCKS_VERSION.tar.gz

RUN cd $LUAROCKS && \
    ./configure --prefix=$LUA_HOME --with-lua=$LUA_HOME && \
    make --quiet bootstrap
