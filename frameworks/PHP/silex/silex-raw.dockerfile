FROM techempower/silex-base:0.1

RUN mv /silex/web/index_raw.php /silex/web/index.php

CMD service php7.2-fpm start && \
    nginx -c /silex/deploy/nginx.conf -g "daemon off;"
