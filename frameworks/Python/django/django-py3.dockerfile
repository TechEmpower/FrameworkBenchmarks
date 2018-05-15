FROM python:3.6.5

ADD ./ /django

WORKDIR /django

RUN pip3 install -r /django/requirements_py3.txt

CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql
