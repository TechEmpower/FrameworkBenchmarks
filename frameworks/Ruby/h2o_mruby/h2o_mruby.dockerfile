FROM ruby:2.4

ENV H2O_HOME=/h2o
ENV VERSION="2.2.4"
ENV ARCHIVE=v${VERSION}.tar.gz
ENV BUILD_DIR=h2o-${VERSION}

RUN mkdir /h2o

WORKDIR /h2o

RUN wget -q https://github.com/h2o/h2o/archive/$ARCHIVE
RUN tar xf $ARCHIVE

WORKDIR /h2o/h2o-2.2.4

RUN apt update -yqq && apt install -yqq make cmake libmcrypt-dev libssl-dev bison
RUN cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
      -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=on
RUN make -j "$(nproc)" install

ENV PATH=/h2o/bin:${PATH}

ADD ./h2o.conf /h2o_mruby/

CMD "${H2O_HOME}/bin/h2o" -c "/h2o_mruby/h2o.conf"
