FROM tfb/php7:latest

ADD ./ /silex
WORKDIR /silex

RUN composer.phar install --no-progress
