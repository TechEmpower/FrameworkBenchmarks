FROM tfb/php7:latest

ADD ./ /zend
WORKDIR /zend

RUN composer.phar install

CMD bash run.sh
