FROM techempower/python3:0.1

ADD ./ /django

WORKDIR /django

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /django/requirements_py3.txt

CMD gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql
