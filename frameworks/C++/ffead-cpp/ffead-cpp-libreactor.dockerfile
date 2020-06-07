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

WORKDIR ${IROOT}
RUN apt-get install -y build-essential libjansson-dev wget
RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v1.3.0/libdynamic-1.3.0.tar.gz \
	&& tar fvxz libdynamic-1.3.0.tar.gz && cd libdynamic-1.3.0 && ./configure --prefix=/usr AR=gcc-ar NM=gcc-nm RANLIB=gcc-ranlib \
	&& make install

WORKDIR ${IROOT}
RUN wget -q https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.1/libreactor-1.0.1.tar.gz \
	&& tar fvxz libreactor-1.0.1.tar.gz && cd libreactor-1.0.1 && ./configure --prefix=/usr AR=gcc-ar NM=gcc-nm RANLIB=gcc-ranlib \
	&& make install

WORKDIR ${IROOT}/lang-server-backends/c/libreactor
RUN make && cp libreactor-ffead-cpp $IROOT/

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-4.0 libreactor
