FROM tfb/php7:latest

ADD ./ /php
WORKDIR /php

RUN composer.phar install --no-progress

RUN chmod -R 777 /php
