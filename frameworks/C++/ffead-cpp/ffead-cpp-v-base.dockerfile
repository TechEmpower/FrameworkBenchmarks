FROM sumeetchhetri/ffead-cpp-4.0-base:1.0
LABEL maintainer="Sumeet Chhetri"
LABEL version="1.0"
LABEL description="Base v docker image with ffead-cpp v4.0 - commit id - 83dd80bcf3c12403e4ba9819496ffcf85acfc43b"

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig
	
RUN apt update -yqq && apt install -y git make && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/vlang/v && cd v && make && ./v symlink

WORKDIR ${IROOT}/lang-server-backends/v/vweb
RUN chmod +x *.sh && ./build.sh && cp vweb $IROOT/

WORKDIR ${IROOT}/lang-server-backends/v/pico.v
RUN chmod +x *.sh && ./build.sh && cp main $IROOT/ && rm -rf ${IROOT}/lang-server-backends

FROM buildpack-deps:bionic
RUN apt update -yqq && apt install --no-install-recommends -yqq uuid-dev odbc-postgresql unixodbc unixodbc-dev memcached \
	libmemcached-dev libssl-dev libhiredis-dev zlib1g-dev libcurl4-openssl-dev redis-server && rm -rf /var/lib/apt/lists/*
COPY --from=0 /installs/ffead-cpp-4.0 /installs/ffead-cpp-4.0
COPY --from=0 /installs/ffead-cpp-4.0-sql /installs/ffead-cpp-4.0-sql
COPY --from=0 /installs/main /installs/
COPY --from=0 /installs/vweb /installs/
RUN mkdir -p /installs/snmalloc-0.4.2/build
COPY --from=0 /installs/snmalloc-0.4.2/build/libsnmallocshim-1mib.so /installs/snmalloc-0.4.2/build/
COPY --from=0 /usr/lib/x86_64-linux-gnu/odbc /usr/lib/x86_64-linux-gnu/odbc
COPY --from=0 /usr/local/lib /usr/local/lib
COPY --from=0 /run_ffead.sh /
