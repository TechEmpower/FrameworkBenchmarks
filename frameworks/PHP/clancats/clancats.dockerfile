FROM tfb/php5:latest

ADD ./ /clancats
WORKDIR /clancats

RUN composer.phar install

RUN git clone --branch v2.0.6 https://github.com/ClanCats/Framework.git clancatsapp
RUN cp -r app/ clancatsapp/CCF/
RUN cp -r vendor/ clancatsapp/CCF/

CMD bash run.sh
