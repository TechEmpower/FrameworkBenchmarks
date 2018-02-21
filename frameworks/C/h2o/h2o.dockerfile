FROM ruby-2.4:latest

COPY ./ ./

RUN apt install -yqq cmake automake libuv1-dev checkinstall autoconf pkg-config libtool python-sphinx libcunit1-dev nettle-dev libyaml-dev

### Install mustache-c

RUN git clone https://github.com/x86-64/mustache-c.git && \
    cd mustache-c && \
    git checkout 55dafd1e95adaca90ea50efb9a8573786514c85a && \
    CFLAGS="-O3 -flto -march=native" ./configure --prefix=/root/mustache-c && \
    make -j "$(nproc)" install

ENV LD_LIBRARY_PATH=/root/mustache-c/lib:${LD_LIBRARY_PATH}

### Install yajl

ENV YAJL_VERSION="2.1.0"
ENV YAJL_ARCHIVE="${YAJL_VERSION}.tar.gz"
ENV YAJL_BUILD_DIR="yajl-${YAJL_VERSION}"

RUN wget https://github.com/lloyd/yajl/archive/${YAJL_ARCHIVE} && \
    tar xvf ${YAJL_ARCHIVE} && \
    cd ${YAJL_BUILD_DIR} && \
    ./configure -p /root/yajl && \
    make -j "$(nproc)" install

ENV LD_LIBRARY_PATH=/root/yajl/lib:${LD_LIBRARY_PATH}

### Install wslay

RUN git clone https://github.com/tatsuhiro-t/wslay.git && \
    cd wslay && \
    autoreconf -i && \
    automake && \
    autoconf && \
    ./configure && \
    make && \
    make install

### Install h2o

ENV H2O_VERSION="2.2.4"
ENV H2O_ARCHIVE="v${H2O_VERSION}.tar.gz"
ENV H2O_BUILD_DIR="h2o-${H2O_VERSION}"

RUN wget https://github.com/h2o/h2o/archive/${H2O_ARCHIVE} && \
    tar xvf ${H2O_ARCHIVE} && \
    cd $H2O_BUILD_DIR && \
    cmake -DCMAKE_INSTALL_PREFIX="/root/h2o" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=on && \
    make -j "$(nproc)" install

ENV PATH=/root/h2o/bin:${PATH}

CMD ["./setup.sh"]
