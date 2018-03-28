FROM tfb/nginx:latest
FROM tfb/cutelyst-benchmark-app:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD nginx.conf /nginx.conf

RUN sed -i "s|include .*/conf/uwsgi_params;|include ${NGINX_HOME}/conf/uwsgi_params;|g" /nginx.conf
RUN sed -i "s|SendDate=.*|SendDate=false|g" /cutelyst_socket.ini
