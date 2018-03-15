FROM tfb/web2py-base:latest

WORKDIR /web2py

CMD gunicorn web2py.wsgi:application -c gunicorn_conf.py
