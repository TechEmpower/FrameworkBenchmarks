FROM ubuntu:16.04

RUN apt update -yqq

RUN apt install -yqq software-properties-common build-essential curl locales wget unzip git \
    libmysqlclient-dev libpq-dev \
    libpcre3 libpcre3-dev \
    libssl-dev libcurl4-openssl-dev \
    zlib1g-dev \
    libreadline6-dev \
    libbz2-dev \
    libxslt-dev libgdbm-dev ncurses-dev  \
    libffi-dev libtool bison libevent-dev \
    libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
    liborc-0.4-0 libgnutls-dev \
    libjson0-dev libmcrypt-dev libicu-dev \
    re2c libnuma-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

ENV QT_VERSION_MM 59
ENV QT_VERSION_FULL 594-xenial
ENV CMAKE_PREFIX_PATH /opt/qt${QT_VERSION_MM}
ENV LD_LIBRARY_PATH ${CMAKE_PREFIX_PATH}/lib

ENV QT_VERSION_MM 59
ENV QT_VERSION_FULL 594-xenial
ENV CMAKE_PREFIX_PATH /opt/qt${QT_VERSION_MM}
ENV LD_LIBRARY_PATH ${CMAKE_PREFIX_PATH}/lib

RUN apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL && \
    apt update -qq && \
    apt install -yqq \
    cmake \
    clearsilver-dev \
    libgrantlee5-dev \
    libjemalloc-dev \
    qt${QT_VERSION_MM}base \
    qt${QT_VERSION_MM}script \
    qt${QT_VERSION_MM}tools

RUN apt install -yqq uwsgi uwsgi uuid-dev libcap-dev libzmq3-dev

ENV CUTELYST_VER 2.0.1

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

ENV TROOT /cutelyst-benchmark-app
ENV LD_LIBRARY_PATH ${CMAKE_PREFIX_PATH}/lib
ENV CUTELYST_APP ${TROOT}/build/libcutelyst_benchmarks.so

COPY src ${TROOT}/
COPY config/config.ini /cutelyst.ini
COPY config/config_socket.ini /cutelyst_socket.ini
COPY nginx.conf /nginx.conf

RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=tfb-database|g" /cutelyst.ini
RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=tfb-database|g" /cutelyst_socket.ini

RUN cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make

RUN apt install -yqq nginx

RUN sed -i "s|SendDate=.*|SendDate=false|g" /cutelyst_socket.ini

ENV C_THREADS 1
ENV CPU_AFFINITY 1

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=$(nproc) \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
