FROM techempower/php7:0.1

ADD ./ /workerman
WORKDIR /workerman

RUN composer.phar install --no-progress

CMD php /workerman/server.php start
