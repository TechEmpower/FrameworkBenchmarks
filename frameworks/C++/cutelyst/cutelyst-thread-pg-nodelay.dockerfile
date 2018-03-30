FROM techempower/cutelyst-benchmark-app:0.1

ENV C_PROCESSES=1
ENV CPU_AFFINITY=1
ENV DRIVER=QPSQL

RUN sed -i "s|Driver=.*|Driver=${DRIVER}|g" /cutelyst.ini

CMD cutelyst-wsgi2 \
    --ini /cutelyst.ini:uwsgi \
    --application ${CUTELYST_APP} \
    --processes=${C_PROCESSES} \
    --threads=$(nproc) \
    --cpu-affinity=${CPU_AFFINITY} \
    --socket-timeout 0 \
    --reuse-port \
    --tcp-nodelay
