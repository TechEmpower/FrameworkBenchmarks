FROM techempower/base:0.1

ENV QT_VERSION_MM=59
ENV QT_VERSION_FULL=594-xenial
ENV CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}
ENV LD_LIBRARY_PATH=${CMAKE_PREFIX_PATH}/lib

RUN apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL && \
    apt-get update -qq && \
    apt-get install -qqy \
    cmake \
    clearsilver-dev \
    libgrantlee5-dev \
    libjemalloc-dev \
    qt${QT_VERSION_MM}base \
    qt${QT_VERSION_MM}script \
    qt${QT_VERSION_MM}tools
    
RUN apt install -yqq uwsgi uwsgi uuid-dev libcap-dev libzmq3-dev
