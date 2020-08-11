FROM buildpack-deps:bionic
LABEL maintainer="Sumeet Chhetri"
LABEL version="1.0"
LABEL description="Base ffead-cpp docker image with commit id - 83dd80bcf3c12403e4ba9819496ffcf85acfc43b"

ENV IROOT=/installs

RUN mkdir /installs
COPY te-benchmark-um/ /installs/te-benchmark-um/

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
