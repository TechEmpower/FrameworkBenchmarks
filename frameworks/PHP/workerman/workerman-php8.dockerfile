FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php8.0 php8.0-common php8.0-cli php8.0-pgsql php8.0-xml > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php8.0-dev libevent-dev > /dev/null
#RUN apt-get install -y php-pear php8.0-dev libevent-dev php8.0-mbstring php8.0-simplexml php8.0-zip > /dev/null
#RUN curl -sSL https://pear.php.net/go-pear.phar -o pear > /dev/null \
#        && pear install event \
#        && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

#RUN curl -sSL https://github.com/FriendsOfPHP/pickle/releases/latest/download/pickle.phar -o /usr/local/bin/pickle \
#        && chmod +x /usr/local/bin/pickle
#RUN pickle install event && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

RUN curl -sSL https://bitbucket.org/osmanov/pecl-event/get/3.0.0-beta.1.zip -o event.zip \
        && unzip -qq event.zip \
        && cd osmanov-pecl-event-efcc0739aac6 \
        && phpize > /dev/null \
        && ./configure --with-event-core --with-event-extra \
        && make > /dev/null && make install > /dev/null \
        && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini
 
COPY php.ini /etc/php/8.0/cli/php.ini

ADD ./ /workerman
WORKDIR /workerman

RUN sed -i "s|'mysql:host|'pgsql:host|g" app.php

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD php /workerman/server.php start
