FROM sumeetchhetri/ffead-cpp-5.0-base:5.2
LABEL maintainer="Sumeet Chhetri"
LABEL version="5.2"
LABEL description="Base rust rocket docker image with ffead-cpp v5.0 - commit id - master"

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
WORKDIR ${IROOT}/lang-server-backends/rust/rocket-ffead-cpp/
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup default nightly && cargo update && cargo build --release && cp target/release/rocket-ffead-cpp $IROOT/ && rm -rf ${IROOT}/lang-server-backends
