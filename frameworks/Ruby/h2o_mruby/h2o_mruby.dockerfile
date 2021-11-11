FROM ruby:2.6

ADD ./h2o.conf ./

RUN apt-get update && apt-get install -yqq bison cmake libssl-dev make

ENV H2O_VERSION=2.3.0-beta2
ENV H2O_ARCHIVE="v${H2O_VERSION}.tar.gz"
ENV H2O_HOME=/h2o

RUN wget -q "https://github.com/h2o/h2o/archive/$H2O_ARCHIVE" && \
    tar xf "$H2O_ARCHIVE" && \
    cd "h2o-$H2O_VERSION" && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=on . && \
    make -j "$(nproc)" install

EXPOSE 8080

CMD "${H2O_HOME}/bin/h2o" -c h2o.conf
