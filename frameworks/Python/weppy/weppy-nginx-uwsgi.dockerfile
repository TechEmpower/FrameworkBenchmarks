FROM python:2.7.14

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /weppy

WORKDIR /weppy

RUN pip install -r /weppy/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /weppy/nginx.conf

CMD nginx -c /weppy/nginx.conf && uwsgi --ini /weppy/uwsgi.ini --processes $(nproc) --wsgi app:app
