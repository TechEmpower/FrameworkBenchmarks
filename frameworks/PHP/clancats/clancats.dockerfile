FROM ubuntu:18.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql php7.3-xml php7.3-mbstring php7.0-mcrypt  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.3/fpm/
RUN sed -i "s|listen = /run/php/php7.3-fpm.sock|listen = /run/php/php7.3-fpm.sock|g" /etc/php/7.3/fpm/php-fpm.conf

ADD ./ /clancats
WORKDIR /clancats

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

RUN composer install --quiet

RUN git clone --branch v2.0.6 --depth 1 https://github.com/ClanCats/Framework.git clancatsapp
RUN cp -r app/ clancatsapp/CCF/
RUN cp -r vendor/ clancatsapp/CCF/

CMD service php7.3-fpm start && \
    nginx -c /clancats/deploy/nginx.conf -g "daemon off;"
