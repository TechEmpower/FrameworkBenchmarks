FROM tfb/cutelyst-benchmark-app:latest

ENV C_PROCESSES=1
ENV C_THREADS=${CPU_COUNT}
ENV CPU_AFFINITY=1

RUN ldd /usr/bin/cutelyst-wsgi2

CMD cutelyst-wsgi2 \
    --ini ${TROOT}/config.ini:uwsgi \
    --application ${TROOT}/build/src/libcutelyst_benchmarks.so \
    --processes=${C_PROCESSES} \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --socket-timeout 0 \
    --reuse-port
