FROM php:7.1

MAINTAINER luoxiaojun1992 <luoxiaojun1992@sina.cn>

# Version
ENV SWOOLE_VERSION v4.4.0

# Libs
RUN apt-get update -yqq \
    && apt-get install -yqq \
        curl wget git zip unzip less vim procps lsof tcpdump htop openssl \
        libz-dev \
        libssl-dev \
        libnghttp2-dev \
        libpcre3-dev \
        libjpeg-dev \
        libpng-dev \
        libfreetype6-dev

# Composer
RUN curl -sS https://getcomposer.org/installer | php \
    && mv composer.phar /usr/local/bin/composer \
    && composer self-update --clean-backups

# PDO extension
RUN docker-php-ext-install pdo_mysql

# Bcmath extension required by amqp composer package
RUN docker-php-ext-install bcmath

# Sockets extension
RUN docker-php-ext-install sockets

# Swoole extension
RUN wget -q https://github.com/swoole/swoole-src/archive/${SWOOLE_VERSION}.tar.gz -O swoole.tar.gz \
    && mkdir -p swoole \
    && tar -xf swoole.tar.gz -C swoole --strip-components=1 \
    && rm swoole.tar.gz \
    && ( \
        cd swoole \
        && phpize \
        && ./configure --enable-mysqlnd --quiet \
        && make -j$(nproc) --quiet \
        && make install --quiet \
    ) \
    && rm -r swoole \
    && docker-php-ext-enable swoole

ADD . /var/www/sw-fw-less

WORKDIR /var/www/sw-fw-less

RUN composer install --no-dev \
    && composer dump-autoload -o \
    && composer clearcache

EXPOSE 9501

ENTRYPOINT ["php", "/var/www/sw-fw-less/start.php"]