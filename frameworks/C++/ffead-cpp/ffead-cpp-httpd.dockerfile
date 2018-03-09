FROM tfb/ffead-cpp-base:latest

COPY ffead-cpp-httpd.sh ./

RUN ./ffead-cpp-httpd.sh

ENV PATH=${IROOT}/httpd/bin:${PATH}
