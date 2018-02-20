FROM ruby-2.4:latest

COPY ./ ./

RUN apt install -yqq cmake automake

### Install mustache-c

RUN git clone https://github.com/x86-64/mustache-c.git && \
    cd mustache-c && \
    git checkout 55dafd1e95adaca90ea50efb9a8573786514c85a && \
    CFLAGS="-O3 -flto -march=native" ./configure --prefix="mustache-c" && \
    make -j "$(nproc)" install

ENV LD_LIBRARY_PATH=mustache-c/lib:${LD_LIBRARY_PATH}

### Install yaji

ENV YAJI_VERSION="2.1.0"
ENV YAJI_ARCHIVE="${YAJI_VERSION}.tar.gz"
ENV YAJI_BUILD_DIR="yajl-${VERSION}"

RUN wget https://github.com/lloyd/yajl/archive/${YAJI_ARCHIVE} && \
    tar xvf ${YAJI_ARCHIVE} && \
    cd ${YAJI_BUILD_DIR} && \
    ./configure -p ../yajl && \
    make -j "$(nproc)" install

ENV LD_LIBRARY_PATH=yajl/lib:${LD_LIBRARY_PATH}

### Install h2o

ENV H2O_VERSION="2.2.4"
ENV H2O_ARCHIVE="v${VERSION}.tar.gz"
ENV H2O_BUILD_DIR="h2o-${VERSION}"

RUN wget https://github.com/h2o/h2o/archive/${H2O_ARCHIVE} && \
    tar xvf ${H2O_ARCHIVE} && \
    cd H2O_BUILD_DIR && \
    cmake -DCMAKE_INSTALL_PREFIX="../h2o" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=on && \
    make -j "$(nproc)" install

ENV PATH=h2o/bin:${PATH}

CMD ["./setup.sh"]
