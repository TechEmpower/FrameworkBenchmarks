FROM buildpack-deps:bionic
LABEL maintainer="Sumeet Chhetri"
LABEL version="1.0"
LABEL description="Base rust docker image with ffead-cpp v4.0"

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

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

RUN cd ${IROOT}/lang-server-backends/rust/actix-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/actix-ffead-cpp $IROOT/ && rm -rf target && \
	cd ${IROOT}/lang-server-backends/rust/hyper-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/hyper-ffead-cpp $IROOT/ && rm -rf target && \
	cd ${IROOT}/lang-server-backends/rust/thruster-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/thruster-ffead-cpp $IROOT/ && rm -rf target && \
	rm -rf ${IROOT}/lang-server-backends && rm -rf /root/.rustup /root/.cargo
