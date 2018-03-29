FROM tfb/nginx:latest

FROM tfb/dart-lang:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /stream
WORKDIR /stream

RUN pub upgrade

RUN chmod -R 777 /stream

CMD ./nginx-conf.sh && \
    ./start-servers.sh && \
    sleep 20 && nginx -c /start/nginx.conf -g "daemon off;"
