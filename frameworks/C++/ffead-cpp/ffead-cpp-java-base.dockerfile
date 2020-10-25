FROM sumeetchhetri/ffead-cpp-5.0-base:5.1
LABEL maintainer="Sumeet Chhetri"
LABEL version="5.1"
LABEL description="Base java docker image with master code"

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig
	
RUN apt update -yqq && apt install -y --no-install-recommends default-jre maven gradle && rm -rf /var/lib/apt/lists/*
RUN cd ${IROOT}/lang-server-backends/java/firenio && mvn compile assembly:single -q && cp target/firenio-ffead-cpp-0.1-jar-with-dependencies.jar $IROOT/
RUN cd ${IROOT}/lang-server-backends/java/rapidoid && mvn compile assembly:single -q && cp target/rapidoid-ffead-cpp-1.0-jar-with-dependencies.jar $IROOT/
RUN cd ${IROOT}/lang-server-backends/java/wizzardo-http && gradle --refresh-dependencies clean fatJar -q && cp build/libs/wizzardo-ffead-cpp-all-1.0.jar $IROOT/
RUN rm -rf ${IROOT}/lang-server-backends

FROM buildpack-deps:bionic
RUN apt update -yqq && apt install --no-install-recommends -yqq uuid-dev odbc-postgresql unixodbc unixodbc-dev memcached \
	libmemcached-dev libssl-dev libhiredis-dev zlib1g-dev libcurl4-openssl-dev redis-server default-jre libpq-dev && rm -rf /var/lib/apt/lists/*
COPY --from=0 /installs/ffead-cpp-5.0 /installs/ffead-cpp-5.0
COPY --from=0 /installs/ffead-cpp-5.0-sql /installs/ffead-cpp-5.0-sql
COPY --from=0 /installs/firenio-ffead-cpp-0.1-jar-with-dependencies.jar /installs/
COPY --from=0 /installs/rapidoid-ffead-cpp-1.0-jar-with-dependencies.jar /installs/
COPY --from=0 /installs/wizzardo-ffead-cpp-all-1.0.jar /installs/
RUN mkdir -p /installs/snmalloc-0.4.2/build
COPY --from=0 /installs/snmalloc-0.4.2/build/libsnmallocshim-1mib.so /installs/snmalloc-0.4.2/build/
COPY --from=0 /usr/lib/x86_64-linux-gnu/odbc /usr/lib/x86_64-linux-gnu/odbc
COPY --from=0 /usr/local/lib /usr/local/lib
COPY --from=0 /run_ffead.sh /
