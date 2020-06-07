FROM buildpack-deps:bionic

ENV IROOT=/installs

RUN mkdir /installs

WORKDIR /

COPY te-benchmark-um/ te-benchmark-um/
COPY *.sh ./
RUN chmod 755 *.sh

RUN ./install_ffead-cpp-dependencies.sh

WORKDIR /

RUN ./install_ffead-cpp-framework.sh

WORKDIR /

RUN ./install_ffead-cpp-httpd.sh

WORKDIR /

RUN ./install_ffead-cpp-nginx.sh

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so
RUN ln -s ${IROOT}/ffead-cpp-4.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so
RUN ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so
RUN ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so
RUN ln -s ${IROOT}/ffead-cpp-4.0/lib/libinter.so /usr/local/lib/libinter.so
RUN ln -s ${IROOT}/ffead-cpp-4.0/lib/libdinter.so /usr/local/lib/libdinter.so
RUN ldconfig

RUN apt-get update -y && apt-get install -yqq libh2o-evloop-dev libwslay-dev libyaml-0-2 libevent-dev libpcre3-dev \
    	gcc wget git libssl-dev libuv1-dev ca-certificates --no-install-recommends
RUN wget -q https://github.com/crystal-lang/crystal/releases/download/0.26.1/crystal-0.26.1-1-linux-x86_64.tar.gz \
	&& tar --strip-components=1 -xzf crystal-0.26.1-1-linux-x86_64.tar.gz -C /usr/ && rm -f *.tar.gz
WORKDIR ${IROOT}/lang-server-backends/crystal/h2o.cr
RUN shards install && gcc -shared -O3 lib/h2o/src/ext/h2o.c -I/usr/include -fPIC -o h2o.o \
	&& CRYSTAL_PATH=lib:/usr/share/crystal/src crystal build --prelude=empty --no-debug --release -Dgc_none -Dfiber_none -Dexcept_none -Dhash_none -Dtime_none -Dregex_none -Dextreme h2o-evloop-ffead-cpp.cr --link-flags="-Wl,-s $PWD/h2o.o -DH2O_USE_LIBUV=0" -o h2o-evloop-ffead-cpp.out \
	&& cp h2o-evloop-ffead-cpp.out $IROOT/

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-4.0 crystal-h2o
