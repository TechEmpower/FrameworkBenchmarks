FROM sumeetchhetri/ffead-cpp-base:6.0

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

RUN wget -q https://dl.google.com/go/go1.14.4.linux-amd64.tar.gz && tar -C /usr/local -xzf go1.14.4.linux-amd64.tar.gz
ENV PATH=$PATH:/usr/local/go/bin
WORKDIR ${IROOT}/lang-server-backends/go/gnet
RUN make && cp gnet-ffead-cpp $IROOT/ && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 go-gnet
