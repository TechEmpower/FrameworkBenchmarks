FROM tfb/pypy2:latest

ADD ./ /falcon

WORKDIR /falcon

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /falcon/requirements-pypy.lock

CMD gunicorn app:app -c gunicorn_conf.py
