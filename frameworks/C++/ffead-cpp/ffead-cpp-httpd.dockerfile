FROM techempower/ffead-cpp-base:0.1

COPY ffead-cpp-httpd.sh ./

RUN chmod 755 *.sh

RUN ./ffead-cpp-httpd.sh

ENV PATH=${IROOT}/httpd/bin:${PATH}
