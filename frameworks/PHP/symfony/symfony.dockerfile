FROM ubuntu:16.04

RUN apt update -yqq && apt install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql php7.2-xml

COPY deploy/conf/* /etc/php/7.2/fpm/

RUN mkdir /composer
WORKDIR /composer

RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php -r "if (hash_file('SHA384', 'composer-setup.php') === '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
RUN php composer-setup.php
RUN php -r "unlink('composer-setup.php');"

ENV PATH /composer:${PATH}

ADD ./ /symfony
WORKDIR /symfony

ENV APP_ENV=prod

RUN composer.phar install --no-progress

RUN php bin/console cache:clear --env=prod --no-debug --no-warmup
RUN php bin/console cache:warmup --env=prod --no-debug

RUN mkdir -p /symfony/var/cache/dev
RUN chmod 777 -R /symfony/var/cache/dev

RUN mkdir -p /symfony/var/log
RUN chmod 777 -R /symfony/var/log

CMD service php7.2-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
