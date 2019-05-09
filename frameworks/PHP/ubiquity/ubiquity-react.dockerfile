FROM ubuntu:18.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip php7.3 php7.3-common php7.3-cli php7.3-mysql php7.3-mbstring > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/php-cli.ini /etc/php/7.3/cli/conf.d/php.ini

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN composer require phpmv/ubiquity-devtools:dev-master phpmv/ubiquity-reactphp:dev-master --quiet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/app/cache/*

RUN chmod 777 -R /ubiquity/.ubiquity/*

CMD /ubiquity/vendor/bin/Ubiquity serve -t=react -p=8085 -h=0.0.0.0
