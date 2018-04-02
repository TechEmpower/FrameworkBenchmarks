FROM techempower/php5:0.1

ADD ./ /cygnite
WORKDIR /cygnite

RUN composer.phar install --no-progress
