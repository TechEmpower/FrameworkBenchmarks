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

RUN apt install -y default-jre gradle
WORKDIR ${IROOT}/lang-server-backends/java/wizzardo-http
RUN gradle fatJar -q && cp build/libs/wizzardo-ffead-cpp-all-1.0.jar $IROOT/

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-4.0 java-wizzardo-http
