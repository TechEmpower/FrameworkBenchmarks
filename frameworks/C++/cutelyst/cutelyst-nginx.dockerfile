FROM tfb/cutelyst-benchmark-app:latest
FROM tfb/nginx:latest

ENV C_PROCESSES=${CPU_COUNT}
ENV C_THREADS=1
ENV CPU_AFFINITY=1

ADD nginx.conf /nginx.conf
ADD config/config_socket.ini /cutelyst_socket.ini

RUN sed -i "s|SendDate=.*|SendDate=false|g" /cutelyst_socket.ini
RUN sed -i "s|include .*/conf/uwsgi_params;|include ${NGINX_HOME}/conf/uwsgi_params;|g" /nginx.conf

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=${C_PROCESSES} \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
