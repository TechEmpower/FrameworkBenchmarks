from hyperf/hyperf:8.3-alpine-v3.19-swoole-v6

RUN docker-php-ext-install pcntl opcache curl > /dev/null