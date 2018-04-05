FROM techempower/php5:0.1

ADD ./ /clancats
WORKDIR /clancats

RUN composer.phar install --no-progress

RUN git clone --branch v2.0.6 https://github.com/ClanCats/Framework.git clancatsapp
RUN cp -r app/ clancatsapp/CCF/
RUN cp -r vendor/ clancatsapp/CCF/

CMD service php5.6-fpm start && \
    nginx -c /clancats/deploy/nginx.conf -g "daemon off;"
