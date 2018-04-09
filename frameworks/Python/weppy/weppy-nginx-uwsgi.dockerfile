FROM python:2.7.14

RUN apt update -yqq && apt -yqq install nginx

ADD ./ /weppy

WORKDIR /weppy

RUN pip install -r /weppy/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include /etc/nginx/uwsgi_params;|g' /weppy/nginx.conf

CMD nginx -c /weppy/nginx.conf && uwsgi --ini /weppy/uwsgi.ini --processes $(nproc) --wsgi app:app
