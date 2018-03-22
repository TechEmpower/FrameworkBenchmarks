FROM tfb/base:latest

ENV QT_VERSION_MM=59
ENV QT_VERSION_FULL=594-xenial
ENV CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}

RUN apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL && \
    apt-get update -qq && \
    apt-get install -qqy \
    cmake \
    uwsgi \
    uuid-dev \
    libcap-dev \
    libzmq3-dev \
    clearsilver-dev \
    libgrantlee5-dev \
    libjemalloc-dev \
    qt${QT_VERSION_MM}base \
    qt${QT_VERSION_MM}script \
    qt${QT_VERSION_MM}tools
