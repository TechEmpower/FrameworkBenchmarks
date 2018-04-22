FROM ubuntu:16.04

RUN apt update -yqq && apt install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq  > /dev/null
RUN apt install -yqq nginx git unzip php5.6 php5.6-common php5.6-cli php5.6-fpm php5.6-mysql php5.6-xml php5.6-mbstring php5.6-mcrypt  > /dev/null

RUN mkdir /composer
WORKDIR /composer

ENV PATH /composer:${PATH}

RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php -r "if (hash_file('SHA384', 'composer-setup.php') === '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
RUN php composer-setup.php
RUN php -r "unlink('composer-setup.php');"

COPY deploy/conf/* /etc/php/5.6/fpm/
RUN sed -i "s|listen = /run/php/php7.2-fpm.sock|listen = /run/php/php5.6-fpm.sock|g" /etc/php/5.6/fpm/php-fpm.conf

ADD ./ /cakephp
WORKDIR /cakephp

RUN mkdir -p app/tmp/cache/models
RUN mkdir -p app/tmp/cache/persistent
RUN mkdir -p app/tmp/logs
RUN chmod -R 777 app/tmp

RUN composer.phar install

CMD service php5.6-fpm start && \
    nginx -c /cakephp/deploy/nginx.conf -g "daemon off;"
