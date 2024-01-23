FROM google/dart:1.24

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /start
WORKDIR /start

RUN pub upgrade

RUN chmod -R 777 /start

EXPOSE 8080

CMD ./nginx-conf.sh && \
    ./start-servers.sh && \
    sleep 20 && nginx -c /start/nginx.conf -g "daemon off;"
