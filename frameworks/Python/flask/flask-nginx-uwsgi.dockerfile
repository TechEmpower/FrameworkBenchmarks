FROM python:3.8-buster

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ buster nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ buster nginx" >> /etc/apt/sources.list

RUN apt-get update -yqq && apt-get install -yqq nginx
RUN apt-get install libpq-dev python3-dev -y
ADD ./requirements.txt /flask/requirements.txt
RUN pip3 install -r /flask/requirements.txt
ADD ./ /flask
WORKDIR /flask

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /flask/nginx.conf

EXPOSE 8080

CMD nginx -c /flask/nginx.conf && uwsgi --ini /flask/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:app
