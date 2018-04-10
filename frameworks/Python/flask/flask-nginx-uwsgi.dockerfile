FROM python:3.6.5

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /flask

WORKDIR /flask

RUN pip3 install -r /flask/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /flask/nginx.conf

CMD nginx -c /flask/nginx.conf && uwsgi --ini /flask/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:app
