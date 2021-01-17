FROM python:3.9.1-buster

ADD ./ /django

WORKDIR /django

RUN pip install -r /django/requirements.txt

EXPOSE 8080

CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql
