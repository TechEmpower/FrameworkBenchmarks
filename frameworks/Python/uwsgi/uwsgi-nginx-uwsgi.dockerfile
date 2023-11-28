FROM python:3.9-bullseye

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ bullseye nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ bullseye nginx" >> /etc/apt/sources.list

RUN apt-get update -yqq && apt-get install -yqq nginx
RUN apt-get install python3-dev -y

ADD ./ /uw

WORKDIR /uw

RUN pip3 install -U pip; pip3 install -r /uw/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /uw/nginx.conf

EXPOSE 8080

CMD nginx -c /uw/nginx.conf && uwsgi --ini uwsgi.ini --processes $(nproc) --gevent 1000 --wsgi hello
