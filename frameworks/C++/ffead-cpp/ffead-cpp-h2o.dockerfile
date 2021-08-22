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

WORKDIR ${IROOT}
RUN apt-get update -y && apt-get install -y --no-install-recommends autoconf bison cmake curl file flex g++ git libnuma-dev libpq-dev \
	libssl-dev libtool libyajl-dev libz-dev make wget && rm -rf /var/lib/apt/lists/*

ARG H2O_VERSION=v2.2.6

ARG H2O_BUILD_DIR=${IROOT}/h2o-build
ENV H2O_PREFIX /opt/h2o

RUN mkdir -p "${H2O_BUILD_DIR}/build" && \
    cd "$H2O_BUILD_DIR" && \
    wget -qO - "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
    tar xz --strip-components=1 && \
    cd build && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_PREFIX" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib .. && \
    make -j "$(nproc)" install && \
    cd ../.. && \
    rm -rf "$H2O_BUILD_DIR"

WORKDIR ${IROOT}/lang-server-backends/c/h2o
RUN chmod +x h2o.sh
#RUN ./h2o.sh && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 h2o
