FROM php:7.3

RUN apt -yqq update
RUN apt -yqq install git

RUN docker-php-ext-install pdo_mysql > /dev/null

RUN pecl install swoole-4.4.6
RUN docker-php-ext-enable swoole

WORKDIR /imi

COPY . /imi

RUN chmod -R ug+rwx /imi/.runtime

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative
RUN composer dumpautoload -o

CMD php vendor/bin/imi server/start -name main
