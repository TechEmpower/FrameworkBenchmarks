FROM techempower/nginx:0.1

FROM google/dart:1.24

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /start
WORKDIR /start

RUN pub upgrade

RUN chmod -R 777 /start

CMD ./nginx-conf.sh && \
    ./start-servers.sh && \
    sleep 20 && nginx -c /start/nginx.conf -g "daemon off;"
