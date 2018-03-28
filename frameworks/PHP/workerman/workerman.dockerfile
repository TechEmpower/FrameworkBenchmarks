FROM tfb/php7:latest

ADD ./ /workerman
WORKDIR /workerman

RUN composer.phar install --no-progress

CMD php /workerman/server.php start
