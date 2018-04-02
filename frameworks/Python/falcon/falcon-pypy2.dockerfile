FROM techempower/pypy2:0.1

ADD ./ /falcon

WORKDIR /falcon

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /falcon/requirements-pypy.lock

CMD gunicorn app:app -c gunicorn_conf.py
