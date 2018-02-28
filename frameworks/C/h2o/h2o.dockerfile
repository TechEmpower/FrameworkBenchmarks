FROM tfb/ruby-2.4:latest

COPY ./ ./

RUN ls

RUN apt install -yqq cmake automake libpq-dev libnuma-dev checkinstall autoconf pkg-config libtool python-sphinx libcunit1-dev nettle-dev libyaml-dev

### Install mustache-c

RUN git clone https://github.com/x86-64/mustache-c.git && \
    cd mustache-c && \
    git checkout 55dafd1e95adaca90ea50efb9a8573786514c85a && \
    CFLAGS="-O3 -flto -march=native" ./configure --prefix=/mustache-c && \
    make -j "$(nproc)" install

ENV MUSTACHE_C_HOME=/mustache-c
ENV LD_LIBRARY_PATH=/mustache-c/lib:${LD_LIBRARY_PATH}

### Install yajl

ENV YAJL_VERSION="2.1.0"
ENV YAJL_ARCHIVE="${YAJL_VERSION}.tar.gz"
ENV YAJL_BUILD_DIR="yajl-${YAJL_VERSION}"

RUN wget https://github.com/lloyd/yajl/archive/${YAJL_ARCHIVE} && \
    tar xvf ${YAJL_ARCHIVE} && \
    cd ${YAJL_BUILD_DIR} && \
    ./configure -p /yajl && \
    make -j "$(nproc)" install

ENV YAJL_HOME=/yajl
ENV LD_LIBRARY_PATH=/yajl/lib:${LD_LIBRARY_PATH}

### Install h2o

ENV IROOT="/install"
ENV H2O_HOME="${IROOT}/h2o"
ENV VERSION="2.2.4"
ENV ARCHIVE="v${VERSION}.tar.gz"
ENV BUILD_DIR="h2o-${VERSION}"

RUN mkdir install
RUN cd "${IROOT}" && \
    wget "https://github.com/h2o/h2o/archive/$ARCHIVE" && \
    tar xvf "$ARCHIVE" && \
    cd "$BUILD_DIR" && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
      -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=on && \
    make -j "$(nproc)" install

ENV PATH=${H2O_HOME}/bin:${PATH}

RUN chmod a+wrx start-servers.sh

CMD ["./start-servers.sh"]
