FROM php:7.3.12

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole && \
    docker-php-ext-install mysqli && \
    mkdir /usr/local/composer && \
    cd /usr/local/composer && \
    php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" && \
    php composer-setup.php && \
    php -r "unlink('composer-setup.php');" && \
    php composer.phar config -g repo.packagist composer https://packagist.phpcomposer.com && \
    apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

WORKDIR /dreamcat
COPY php.ini /usr/local/etc/php/
COPY swoole.php swoole.php
COPY src /dreamcat/src
COPY configs /dreamcat/configs
COPY composer.json composer.json
RUN php /usr/local/composer/composer.phar install --no-dev -o
RUN chmod -R 777 /dreamcat

CMD php swoole.php
