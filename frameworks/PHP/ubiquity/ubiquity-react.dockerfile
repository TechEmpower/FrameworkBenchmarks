FROM ubuntu:19.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip php7.3 php7.3-common php7.3-cli php7.3-mysql php7.3-mbstring php7.3-cgi > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN ln -s /usr/lib/cgi-bin/php /usr/bin/php7.3-cgi

COPY deploy/conf/php.ini /etc/php/7.3/cgi/php.ini

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN composer require phpmv/ubiquity-php-pm:dev-master --quiet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/app/cache/*

RUN chmod 777 -R /ubiquity/.ubiquity/*

RUN chmod 777 /ubiquity/vendor/bin/ppm

CMD /ubiquity/vendor/bin/ppm --bridge='\PHPPM\Ubiquity' --bootstrap='\PHPPM\Ubiquity' start --debug 0 --logging 0 --workers 512 --max-requests 1024 --host=0.0.0.0 --port=8080
