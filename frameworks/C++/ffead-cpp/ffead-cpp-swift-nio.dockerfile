FROM sumeetchhetri/ffead-cpp-5.0-base:5.3

ENV IROOT=/installs

ENV DEBIAN_FRONTEND noninteractive
RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

WORKDIR ${IROOT}
RUN apt-get update -y && apt-get install -y --no-install-recommends clang libicu-dev libpython2.7-dev libtinfo5 libncurses5 libz3-dev \
	 && rm -rf /var/lib/apt/lists/*
	 
RUN wget -q https://swift.org/builds/swift-5.3-release/ubuntu2004/swift-5.3-RELEASE/swift-5.3-RELEASE-ubuntu20.04.tar.gz
RUN tar -xzf swift-5.3-RELEASE-ubuntu20.04.tar.gz
RUN mv swift-5.3-RELEASE-ubuntu20.04 /opt/ && rm -f swift-5.3-RELEASE-ubuntu20.04.tar.gz
RUN ln -s /opt/swift-5.3-RELEASE-ubuntu20.04 /opt/swift

ENV PATH=/opt/swift/usr/bin:${PATH}

WORKDIR ${IROOT}/lang-server-backends/swift/swift-nio/app
RUN swift build --enable-test-discovery -c release -Xlinker "-L/usr/local/lib" -Xlinker "-lffead-framework" \
	&& mv .build/release/app ${IROOT}/ && chmod +x ${IROOT}/app && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-5.0 swift-nio
