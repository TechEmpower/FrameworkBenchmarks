FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq git unzip php7.2 php7.2-common php7.2-cli php7.2-mbstring composer > /dev/null

ADD ./ /amp
WORKDIR /amp

RUN composer install --quiet

RUN chmod -R 777 /amp

CMD php /amp/vendor/bin/cluster -s /amp/server.php
