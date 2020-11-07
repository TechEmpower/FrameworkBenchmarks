FROM php:7.4

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

# Composer
RUN curl -sS https://getcomposer.org/installer | php \
    && mv composer.phar /usr/local/bin/composer \
    && composer self-update --clean-backups

# Bcmath extension required by amqp composer package
RUN docker-php-ext-install bcmath > /dev/null

# Sockets extension
RUN docker-php-ext-install sockets > /dev/null

ADD . /var/www/sw-fw-less

WORKDIR /var/www/sw-fw-less

RUN composer install --no-dev --quiet > /dev/null \
    && composer dump-autoload -o

EXPOSE 9501

ENTRYPOINT ["php", "/var/www/sw-fw-less/start.php"]

LABEL luoxiaojun1992 <luoxiaojun1992@sina.cn>