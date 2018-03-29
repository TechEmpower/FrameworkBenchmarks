FROM techempower/base:0.2

ENV LUA_VERSION="5.1"
ENV LUA_MICRO="5"

RUN apt install -yqq libreadline-dev lib32ncurses5-dev

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
