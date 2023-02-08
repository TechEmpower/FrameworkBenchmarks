ARG RUBY_IMAGE_VERSION=3.2

ARG H2O_PREFIX=/opt/h2o

FROM ruby:${RUBY_IMAGE_VERSION} AS compile

ARG H2O_VERSION=9ab3feb4d7429ddda52a3cf84bd6da0e890bd52a

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && apt-get -yqq install cmake ninja-build
WORKDIR h2o-build
ARG H2O_ARCHIVE="${H2O_VERSION}.tar.gz"
ADD "https://github.com/h2o/h2o/archive/${H2O_ARCHIVE}" ./
RUN tar --strip-components=1 -xf "${H2O_ARCHIVE}"
ARG H2O_PREFIX
WORKDIR build
RUN cmake -G Ninja -DCMAKE_C_FLAGS="-flto -march=native -mtune=native" -DWITH_MRUBY=on \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
          -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" .. && \
    cmake --build . -j && \
    cmake --install .
WORKDIR /

FROM ruby:${RUBY_IMAGE_VERSION}-slim

ARG H2O_PREFIX
ADD ./h2o.conf "${H2O_PREFIX}/"
COPY --from=compile "${H2O_PREFIX}" "${H2O_PREFIX}/"
EXPOSE 8080

CMD ["/opt/h2o/bin/h2o", "-c", "/opt/h2o/h2o.conf"]
