FROM tfb/cutelyst-nginx-base:latest

ENV C_PROCESSES=${CPU_COUNT}
ENV C_THREADS=1
ENV CPU_AFFINITY=1
ENV DRIVER=QMYSQL

RUN sed -i "s|Driver=.*|Driver=${DRIVER}|g" /cutelyst_socket.ini

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=${C_PROCESSES} \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
