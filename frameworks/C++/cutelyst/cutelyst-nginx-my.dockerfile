FROM techempower/cutelyst-shared-setup:latest

RUN apt-get install -yqq nginx

RUN sed -i "s|SendDate=.*|SendDate=false|g" /cutelyst_socket.ini

ENV C_THREADS 1
ENV CPU_AFFINITY 1
ENV DRIVER QMYSQL

RUN sed -i "s|Driver=.*|Driver=${DRIVER}|g" /cutelyst_socket.ini

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=$(nproc) \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
