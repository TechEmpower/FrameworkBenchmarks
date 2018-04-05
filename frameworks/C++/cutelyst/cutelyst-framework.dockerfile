FROM techempower/cutelyst-deps:0.1

ENV CUTELYST_VER=2.0.1

RUN wget -q https://github.com/cutelyst/cutelyst/archive/v$CUTELYST_VER.tar.gz -O cutelyst-$CUTELYST_VER.tar.gz && \
    tar zxf cutelyst-$CUTELYST_VER.tar.gz && \
    cd cutelyst-$CUTELYST_VER && mkdir build && cd build && \
    cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DPLUGIN_UWSGI=on \
    -DPLUGIN_VIEW_GRANTLEE=on \
    -DUSE_JEMALLOC=on && \
    make && make install
