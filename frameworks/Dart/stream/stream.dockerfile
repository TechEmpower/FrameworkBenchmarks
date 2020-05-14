FROM google/dart:1.24

RUN apt-get update -yqq && apt-get install -yqq nginx

ADD ./ /stream
WORKDIR /stream

RUN pub upgrade

RUN chmod -R 777 /stream

CMD ./nginx-conf.sh && \
    ./start-servers.sh && \
    sleep 20 && nginx -c /stream/nginx.conf -g "daemon off;"
