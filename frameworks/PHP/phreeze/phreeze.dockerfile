FROM tfb/php5:latest

ADD ./ /phreeze
WORKDIR /phreeze

RUN composer.phar install --no-progress

CMD service php5.6-fpm start && \
    nginx -c /phreeze/deploy/nginx.conf -g "daemon off;"
