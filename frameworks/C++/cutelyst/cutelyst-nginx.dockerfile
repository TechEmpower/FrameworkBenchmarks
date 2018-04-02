FROM techempower/cutelyst-nginx-base:0.1

ENV C_THREADS=1
ENV CPU_AFFINITY=1

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=$(nproc) \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
