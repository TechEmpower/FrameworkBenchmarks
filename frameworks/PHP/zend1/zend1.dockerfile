FROM tfb/php7:latest

ADD ./ /zend1
WORKDIR /zend1

RUN composer.phar install

CMD bash run.sh
