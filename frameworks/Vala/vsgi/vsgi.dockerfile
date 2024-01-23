FROM ubuntu:22.04

RUN apt-get update -yqq && \
    apt-get install -yqq libglib2.0-dev libsoup2.4-dev libjson-glib-dev \
    build-essential curl valac meson ninja-build

#--------------------------------------
# valum framework
#

ENV VALUM_VERSION 0.3.18

WORKDIR /valum_framework

RUN curl -sL https://github.com/valum-framework/valum/archive/v${VALUM_VERSION}.tar.gz | tar xz
WORKDIR valum-${VALUM_VERSION}
RUN rm -rf build
RUN meson --prefix=/usr --buildtype=release build .
RUN ninja -C build
RUN ninja -C build install

# -rpath dosen't work on 22.04 (see https://github.com/valum-framework/valum/issues/224)
ENV VSGI_SERVER_PATH=/usr/lib/x86_64-linux-gnu/vsgi-0.3/servers

ADD ./ /vsgi_app
WORKDIR /vsgi_app

RUN meson --buildtype=release build .
RUN ninja -C build

EXPOSE 8080

CMD bash run.sh
