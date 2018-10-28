FROM python:2.7.15-stretch

ADD ./ /django

WORKDIR /django

RUN pip install -r /django/requirements.txt

WORKDIR /django

CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql_psycopg2
