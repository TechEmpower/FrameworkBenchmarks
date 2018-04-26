FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common  > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql  > /dev/null

RUN mkdir /composer
WORKDIR /composer

ENV PATH /composer:${PATH}

RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php -r "if (hash_file('SHA384', 'composer-setup.php') === '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
RUN php composer-setup.php
RUN php -r "unlink('composer-setup.php');"

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /fat-free
WORKDIR /fat-free

ENV F3DIR="/fat-free/src"

RUN git clone "https://github.com/bcosca/fatfree-core.git" src
RUN cd src && git checkout -q "069ccd84afd2461c7ebb67f660c142f97577e661" # v3.5.2-dev

RUN chmod -R 777 /fat-free

CMD service php7.2-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf -g "daemon off;"
