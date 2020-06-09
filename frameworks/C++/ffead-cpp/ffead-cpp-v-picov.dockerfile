FROM buildpack-deps:bionic

ENV IROOT=/installs

COPY te-benchmark-um/ te-benchmark-um/
COPY *.sh ./

RUN mkdir /installs && chmod 755 *.sh && /install_ffead-cpp-dependencies.sh && /install_ffead-cpp-framework.sh && \
	/install_ffead-cpp-httpd.sh && /install_ffead-cpp-nginx.sh && rm -rf ${IROOT}/ffead-cpp-src

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-4.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

WORKDIR ${IROOT}
RUN apt update -y && apt install --no-install-recommends -y git && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/vlang/v
WORKDIR ${IROOT}/v
RUN make && ./v symlink
WORKDIR ${IROOT}/lang-server-backends/v/pico.v
RUN chmod +x *.sh && ./build.sh && cp main $IROOT/ && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-4.0 v-picov
