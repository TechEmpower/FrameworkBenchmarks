FROM buildpack-deps:bionic
LABEL maintainer="Sumeet Chhetri"
LABEL version="latest"
LABEL description="Base ffead-cpp docker image with commit id - 5f62633149d832c5608c64fd4a1097fb6ebf6f5c"

ENV IROOT=/installs

RUN mkdir /installs
COPY te-benchmark-um/ /installs/te-benchmark-um/
COPY te-benchmark-um-pq/ /installs/te-benchmark-um-pq/
COPY te-benchmark-um-mgr/ /installs/te-benchmark-um-mgr/

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
