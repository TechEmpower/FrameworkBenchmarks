FROM ubuntu:16.04

RUN apt update -yqq && apt install -yqq software-properties-common apt-transport-https
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt-get update -yqq
RUN apt-get install -yqq hhvm nginx git unzip php5.6 php5.6-common php5.6-cli php5.6-fpm php5.6-mysql php5.6-xml php5.6-mbstring php5.6-mcrypt

RUN mkdir /composer
WORKDIR /composer

RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php -r "if (hash_file('SHA384', 'composer-setup.php') === '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
RUN php composer-setup.php
RUN php -r "unlink('composer-setup.php');"

ENV PATH /composer:${PATH}

ADD ./ /slim
WORKDIR /slim

RUN composer.phar install --no-progress

RUN chmod -R 777 /slim

CMD hhvm -m daemon --config /slim/deploy/config.hdf && \
    nginx -c /slim/deploy/nginx-hhvm.conf -g "daemon off;"
