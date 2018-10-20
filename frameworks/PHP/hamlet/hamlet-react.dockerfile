FROM php:7.2

RUN docker-php-ext-install mysqli && docker-php-ext-enable mysqli

RUN apt-get update && apt-get install -y git unzip
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /hamlet
WORKDIR /hamlet

RUN chmod -R 777 /hamlet
RUN composer install --quiet --classmap-authoritative --no-dev

CMD php react.php
