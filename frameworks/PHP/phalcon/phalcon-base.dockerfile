FROM tfb/php7:latest

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt-get install -y php7.2-phalcon

RUN composer.phar install --no-progress
