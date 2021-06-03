FROM sumeetchhetri/ffead-cpp-base:6.0

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

WORKDIR ${IROOT}
RUN apt-get update -y && apt-get install -y --no-install-recommends build-essential libjansson-dev wget && rm -rf /var/lib/apt/lists/*
RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v1.3.0/libdynamic-1.3.0.tar.gz \
	&& tar fvxz libdynamic-1.3.0.tar.gz && cd libdynamic-1.3.0 && ./configure --prefix=/usr AR=gcc-ar NM=gcc-nm RANLIB=gcc-ranlib \
	&& make install && rm -rf ${IROOT}/libdynamic-1.3.0 && rm -f ${IROOT}/libdynamic-1.3.0.tar.gz && \
	cd /installs && wget -q https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.1/libreactor-1.0.1.tar.gz \
	&& tar fvxz libreactor-1.0.1.tar.gz && cd libreactor-1.0.1 && ./configure --prefix=/usr AR=gcc-ar NM=gcc-nm RANLIB=gcc-ranlib \
	&& make install && rm -rf ${IROOT}/libreactor-1.0.1 && rm -f ${IROOT}/libreactor-1.0.1.tar.gz

WORKDIR ${IROOT}/lang-server-backends/c/libreactor
RUN make && cp libreactor-ffead-cpp $IROOT/ && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 libreactor
