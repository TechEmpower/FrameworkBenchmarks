FROM techempower/php7:0.1

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt install -yqq php7.2-phalcon php7.2-dev

ENV VERSION="3.3.1"

RUN wget -q https://github.com/phalcon/cphalcon/archive/v${VERSION}.tar.gz
RUN tar xf v${VERSION}.tar.gz
RUN cd cphalcon-${VERSION}/build && ./install

RUN mv /phalcon/public/index-micro.php /phalcon/public/index.php

RUN chmod -R 777 app

CMD service php7.2-fpm start && \
    nginx -c /phalcon/deploy/nginx.conf -g "daemon off;"
