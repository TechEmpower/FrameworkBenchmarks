FROM sumeetchhetri/ffead-cpp-base:6.0

ENV IROOT=/installs

ENV DEBIAN_FRONTEND noninteractive
RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

ENV LDC_VERSION 1.24.0
ENV LDC_PATH /usr/local/ldc

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl wget gcc ca-certificates xz-utils libxml2 make git zlib1g-dev && \
    rm -rf /var/lib/apt/lists/* /var/cache/apt/*

RUN set -ex && \
        wget -q https://github.com/ldc-developers/ldc/releases/download/v${LDC_VERSION}/ldc2-${LDC_VERSION}-linux-x86_64.tar.xz && \
        tar xf ldc2-${LDC_VERSION}-linux-x86_64.tar.xz && \
        mv ldc2-${LDC_VERSION}-linux-x86_64 ${LDC_PATH} && \
        rm -rf ldc2*

ENV PATH="${LDC_PATH}/bin:${PATH}"
ENV LIBRARY_PATH="${LDC_PATH}/lib:${LIBRARY_PATH}"
ENV LD_LIBRARY_PATH="${LDC_PATH}/lib:/usr/local/lib:${LD_LIBRARY_PATH}"

RUN chmod 755 -R $LDC_PATH

WORKDIR ${IROOT}/lang-server-backends/d/hunt
RUN dub upgrade --verbose && dub build --build=release --arch=x86_64 --compiler=ldc2 -c=minihttp -f && cp hunt-minihttp ${IROOT}/ \
	&& chmod +x ${IROOT}/hunt-minihttp && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 d-hunt
