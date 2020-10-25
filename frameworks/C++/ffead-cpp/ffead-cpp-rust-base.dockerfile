FROM sumeetchhetri/ffead-cpp-5.0-base:5.1
LABEL maintainer="Sumeet Chhetri"
LABEL version="5.1"
LABEL description="Base rust docker image with ffead-cpp v4.0 - commit id - master"

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-5.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

RUN cd ${IROOT}/lang-server-backends/rust/actix-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/actix-ffead-cpp $IROOT/ && rm -rf target && \
	cd ${IROOT}/lang-server-backends/rust/hyper-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/hyper-ffead-cpp $IROOT/ && rm -rf target && \
	cd ${IROOT}/lang-server-backends/rust/thruster-ffead-cpp && RUSTFLAGS="-C target-cpu=native" cargo build --release && cp target/release/thruster-ffead-cpp $IROOT/ && rm -rf target && \
	rm -rf ${IROOT}/lang-server-backends && rm -rf /root/.rustup /root/.cargo

FROM buildpack-deps:bionic
RUN apt update -yqq && apt install --no-install-recommends -yqq uuid-dev odbc-postgresql unixodbc unixodbc-dev memcached \
	libmemcached-dev libssl-dev libhiredis-dev zlib1g-dev libcurl4-openssl-dev redis-server libpq-dev && rm -rf /var/lib/apt/lists/*
COPY --from=0 /installs/ffead-cpp-5.0 /installs/ffead-cpp-5.0
COPY --from=0 /installs/ffead-cpp-5.0-sql /installs/ffead-cpp-5.0-sql
COPY --from=0 /installs/actix-ffead-cpp /installs/
COPY --from=0 /installs/hyper-ffead-cpp /installs/
COPY --from=0 /installs/thruster-ffead-cpp /installs/
RUN mkdir -p /installs/snmalloc-0.4.2/build
COPY --from=0 /installs/snmalloc-0.4.2/build/libsnmallocshim-1mib.so /installs/snmalloc-0.4.2/build
COPY --from=0 /usr/lib/x86_64-linux-gnu/odbc /usr/lib/x86_64-linux-gnu/odbc
COPY --from=0 /usr/local/lib /usr/local/lib
COPY --from=0 /run_ffead.sh /
