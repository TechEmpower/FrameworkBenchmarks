FROM techempower/django-base:0.1

WORKDIR /django

CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql_psycopg2
