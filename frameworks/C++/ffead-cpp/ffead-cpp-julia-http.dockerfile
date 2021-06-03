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
#RUN apt-get update -y && apt-get install -y --no-install-recommends julia
RUN wget --no-check-certificate -q https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.2-linux-x86_64.tar.gz
RUN tar -xzf julia-1.5.2-linux-x86_64.tar.gz
RUN mv julia-1.5.2 /opt/
RUN rm -f julia-1.5.2-linux-x86_64.tar.gz
ENV PATH="/opt/julia-1.5.2/bin:${PATH}"

RUN julia -e 'import Pkg; Pkg.update()' && \
    julia -e 'import Pkg; Pkg.add("HTTP")' && \
    julia -e 'import Pkg; Pkg.precompile()'

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 julia-http
