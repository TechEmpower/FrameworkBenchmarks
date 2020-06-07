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
RUN apt update -y && apt install -y git
RUN git clone https://github.com/vlang/v
WORKDIR ${IROOT}/v
RUN make && ./v symlink
WORKDIR ${IROOT}/lang-server-backends/v/vweb
RUN chmod +x *.sh && ./build.sh && cp vweb $IROOT/

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-4.0 v-vweb
