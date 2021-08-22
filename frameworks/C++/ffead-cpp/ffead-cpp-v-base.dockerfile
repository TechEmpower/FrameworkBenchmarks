FROM sumeetchhetri/ffead-cpp-base:6.0
LABEL maintainer="Sumeet Chhetri"
LABEL version="6.0"
LABEL description="Base v docker image with ffead-cpp v4.0 commit id - master"

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig
	
RUN apt update -yqq && apt install -y git make && rm -rf /var/lib/apt/lists/*
#For Latest vlang, uncomment the below mentioned line, due to lot of new enhancements and unsafe block handling, vlnag has slowed down tremendously
#RUN git clone https://github.com/vlang/v && cd v && make && ./v symlink

#For the fastest vlang performance, use 0.1.29, where the unsafe changes were only restricted to pointer arithmetic
RUN wget -q https://github.com/vlang/v/releases/download/0.1.29/v_linux.zip && unzip -q v_linux.zip && cd v && chmod +x v && ./v symlink && cd .. && rm -f v_linux.zip

WORKDIR ${IROOT}/lang-server-backends/v/vweb
#COPY vweb.v ${IROOT}/lang-server-backends/v/vweb/
#RUN chmod +x *.sh && ./build.sh && cp vweb $IROOT/

WORKDIR ${IROOT}/lang-server-backends/v/pico.v
#COPY main.v ${IROOT}/lang-server-backends/v/pico.v/
RUN chmod +x *.sh && ./build.sh && cp main $IROOT/ && rm -rf ${IROOT}/lang-server-backends
