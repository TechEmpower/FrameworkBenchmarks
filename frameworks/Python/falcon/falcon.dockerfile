FROM tfb/python2:latest

ADD ./ /falcon

WORKDIR /falcon

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /falcon/requirements.lock

CMD gunicorn app:app -c gunicorn_conf.py
