FROM ubuntu:16.04

RUN apt-get update -yqq && \
    apt-get install -yqq flex libglib2.0-dev libsoup2.4-dev libjson-glib-dev python3-pip \
      build-essential unzip wget curl bison

#--------------------------------------
# vala
#

ENV VALA_API_VERSION 0.36
ENV VALA_VERSION 0.36.3

WORKDIR /vala

RUN curl -sL -O https://download.gnome.org/sources/vala/${VALA_API_VERSION}/vala-${VALA_VERSION}.tar.xz
RUN tar xf vala-${VALA_VERSION}.tar.xz
WORKDIR vala-${VALA_VERSION}
RUN ./configure --prefix=/vala
RUN make
RUN make install
RUN ln -s /vala/share/vala-${VALA_API_VERSION}/vapi /vala/share/vala/vapi

ENV LD_LIBRARY_PATH /vala/lib:${LD_LIBRARY_PATH}
ENV PKG_CONFIG_PATH /vala/lib/pkgconfig:${PKG_CONFIG_PATH}
ENV PATH /vala/bin:${PATH}

#--------------------------------------
# meson
#

ENV MESON_VERSION 0.40.1

RUN pip3 install meson==${MESON_VERSION}

#--------------------------------------
# ninja
#

ENV NINJA_VERSION 1.7.2

WORKDIR /ninja

RUN curl -sL -O https://github.com/ninja-build/ninja/releases/download/v${NINJA_VERSION}/ninja-linux.zip
RUN unzip ninja-linux.zip -d /ninja/bin
ENV PATH /ninja/bin:${PATH}

#--------------------------------------
# valum framework
#

ENV VALUM_VERSION 0.3.12

WORKDIR /valum_framework

RUN curl -sL https://github.com/valum-framework/valum/archive/v${VALUM_VERSION}.tar.gz | tar xz
WORKDIR valum-${VALUM_VERSION}
RUN rm -rf build
RUN meson --prefix=/vala --buildtype=release --libdir=lib build
RUN ninja -C build
RUN ninja -C build install

ADD ./ /vsgi_app
WORKDIR /vsgi_app

RUN meson --buildtype=release build
RUN ninja -C build

EXPOSE 8080

CMD bash run.sh
