FROM tfb/php5:latest

ADD ./ /cakephp
WORKDIR /cakephp

RUN composer.phar install

CMD bash run.sh
