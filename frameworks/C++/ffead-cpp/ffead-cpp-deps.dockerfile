FROM ubuntu:20.04
LABEL maintainer="Sumeet Chhetri"
LABEL version="6.0"
LABEL description="ffead-cpp docker image with backend dependencies"

ENV IROOT=/installs
ENV DEBUG=off

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN mkdir /installs

WORKDIR ${IROOT}

COPY install_ffead-cpp-dependencies.sh ${IROOT}/
RUN chmod 755 ${IROOT}/install_ffead-cpp-dependencies.sh
RUN ./install_ffead-cpp-dependencies.sh

COPY install_ffead-cpp-backends.sh ${IROOT}/
RUN chmod 755 ${IROOT}/install_ffead-cpp-backends.sh
RUN ./install_ffead-cpp-backends.sh
