FROM techempower/php7:0.1

ADD ./ /symfony
WORKDIR /symfony

ENV APP_ENV=prod

RUN composer.phar install --no-progress

RUN php bin/console cache:clear --env=prod --no-debug --no-warmup
RUN php bin/console cache:warmup --env=prod --no-debug

RUN mkdir -p /symfony/var/cache/dev
RUN chmod 777 -R /symfony/var/cache/dev

RUN mkdir -p /symfony/var/log
RUN chmod 777 -R /symfony/var/log
