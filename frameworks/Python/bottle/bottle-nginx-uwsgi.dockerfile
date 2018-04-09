FROM python:3.6.5

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /bottle

WORKDIR /bottle

RUN pip3 install -r /bottle/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /bottle/nginx.conf

CMD nginx -c /bottle/nginx.conf && uwsgi --ini /bottle/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:app
