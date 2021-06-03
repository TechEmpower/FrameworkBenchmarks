FROM sumeetchhetri/ffead-cpp-base:6.0

ENV IROOT=/installs

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

RUN apt-get update -y && apt install -y --no-install-recommends gnupg && curl -sL "https://keybase.io/crystal/pgp_keys.asc" | apt-key add - \
	&& echo "deb https://dist.crystal-lang.org/apt crystal main" | tee /etc/apt/sources.list.d/crystal.list \
	&& apt-get update -y && apt install -y --no-install-recommends crystal && rm -rf /var/lib/apt/lists/*
WORKDIR ${IROOT}/lang-server-backends/crystal/crystal
RUN crystal build --release --no-debug crystal-ffead-cpp.cr -o crystal-ffead-cpp.out && cp crystal-ffead-cpp.out $IROOT/ && rm -rf ${IROOT}/lang-server-backends

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 crystal-http
