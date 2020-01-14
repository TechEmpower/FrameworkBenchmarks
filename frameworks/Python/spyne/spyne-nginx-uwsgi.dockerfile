FROM python:3.6.6-stretch

RUN curl -s http://nginx.org/keys/nginx_signing.key | apt-key add -
RUN echo "deb http://nginx.org/packages/debian/ stretch nginx" >> /etc/apt/sources.list
RUN echo "deb-src http://nginx.org/packages/debian/ stretch nginx" >> /etc/apt/sources.list

RUN apt update -yqq && apt install -yqq nginx

ADD ./ /spyne

WORKDIR /spyne

RUN pip3 install -r /spyne/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /spyne/nginx.conf

CMD nginx -c /spyne/nginx.conf && uwsgi --ini /spyne/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:application
