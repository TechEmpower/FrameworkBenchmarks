FROM tfb/php5:latest

ADD ./ /cygnite
WORKDIR /cygnite

RUN composer.phar install --no-progress
