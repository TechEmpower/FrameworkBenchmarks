FROM techempower/web2py-base:0.1

WORKDIR /web2py

CMD gunicorn web2py.wsgi:application -c gunicorn_conf.py
