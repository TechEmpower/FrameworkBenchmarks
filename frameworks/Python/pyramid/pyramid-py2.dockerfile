FROM tfb/python2:latest

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /pyramid/requirements.txt

CMD gunicorn wsgi:app -c gunicorn_conf.py
