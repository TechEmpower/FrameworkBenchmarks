FROM python:3.6.5

WORKDIR /bottle
COPY views views
COPY app.py app.py
COPY nginx.conf nginx.conf
COPY requirements.txt requirements.txt
COPY uwsgi.ini uwsgi.ini

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ jessie nginx" >> /etc/apt/sources.list

RUN apt update -yqq && apt install -yqq nginx

RUN pip3 install -r requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /bottle/nginx.conf

CMD nginx -c /bottle/nginx.conf && uwsgi --ini /bottle/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:app
