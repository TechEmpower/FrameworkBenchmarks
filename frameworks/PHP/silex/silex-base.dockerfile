FROM techempower/php7:0.1

ADD ./ /silex
WORKDIR /silex

RUN composer.phar install --no-progress
