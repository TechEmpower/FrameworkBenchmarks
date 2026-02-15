FROM phpswoole/swoole:php8.5

ENV ENABLE_COROUTINE 1
ENV CPU_MULTIPLES 1
ENV DATABASE_DRIVER mysql

ARG DEBIAN_FRONTEND=noninteractive

RUN php --ri swoole

WORKDIR /swoole
COPY src .

COPY override.ini /usr/local/etc/php/conf.d/
#RUN php -i 

EXPOSE 8080
CMD ["php", "/swoole/swoole-server.php"]
