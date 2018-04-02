FROM techempower/php5:0.1

ADD ./ /php
WORKDIR /php

RUN composer.phar install --no-progress

RUN chmod -R 777 /php
