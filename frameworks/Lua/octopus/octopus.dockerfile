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

WORKDIR /octo

RUN git clone https://github.com/cyberz-eu/octopus.git

WORKDIR /octo/octopus
# Dec 8th, 2017
RUN git checkout 44c7e7ecdfd9e95703e73df85815c0cca4b441e8

WORKDIR /octo

ADD ./ /octo

RUN cp -avr app octopus/extensions
RUN cp -vf config.lua octopus/extensions

WORKDIR /octo/octopus/bin/unix

RUN chmod +x *.sh

RUN sed -i 's|wget|wget -q|g' server.sh
RUN sed -i 's|-c nginx.conf|-c nginx.conf -g "daemon off;"|g' server.sh

RUN ./server.sh install
RUN ./server.sh build

EXPOSE 8080

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|DBHOSTNAME|$DBIP|g" /octo/octopus/extensions/build/src/types.lua && \
    ./server.sh start
