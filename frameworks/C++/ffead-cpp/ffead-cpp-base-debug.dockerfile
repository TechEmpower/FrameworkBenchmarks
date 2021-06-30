FROM ubuntu:20.04
LABEL maintainer="Sumeet Chhetri"
LABEL version="6.0-debug"
LABEL description="Base ffead-cpp docker image with commit id - master"

ENV IROOT=/installs
ENV DEBUG=on

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN mkdir /installs
COPY te-benchmark-um/ /installs/te-benchmark-um/
COPY te-benchmark-um-pq/ /installs/te-benchmark-um-pq/
COPY te-benchmark-um-mgr/ /installs/te-benchmark-um-mgr/
COPY te-benchmark-um-mgr/ /installs/te-benchmark-um-pq-async/

WORKDIR ${IROOT}

COPY install_ffead-cpp-dependencies.sh ${IROOT}/
RUN chmod 755 ${IROOT}/install_ffead-cpp-dependencies.sh
RUN ./install_ffead-cpp-dependencies.sh

COPY install_ffead-cpp-backends.sh ${IROOT}/
RUN chmod 755 ${IROOT}/install_ffead-cpp-backends.sh
RUN ./install_ffead-cpp-backends.sh

COPY install_ffead-cpp-framework.sh install_ffead-cpp-httpd.sh install_ffead-cpp-nginx.sh server.sh ${IROOT}/
RUN chmod 755 ${IROOT}/*.sh
RUN ./install_ffead-cpp-framework.sh && ./install_ffead-cpp-httpd.sh && ./install_ffead-cpp-nginx.sh && cd ${IROOT}/ffead-cpp-src && make clean && rm -rf CMakeFiles

COPY run_ffead.sh /
RUN chmod 755 /run_ffead.sh

RUN apt update -yqq && apt install -yqq gdb net-tools vim
