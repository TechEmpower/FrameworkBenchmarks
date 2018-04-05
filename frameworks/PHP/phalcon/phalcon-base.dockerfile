FROM techempower/php7:0.1

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt-get install -y php7.2-phalcon

RUN composer.phar install --no-progress

RUN chmod -R 777 app
