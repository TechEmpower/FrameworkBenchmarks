FROM tfb/pypy2:latest

ADD ./ /bottle

WORKDIR /bottle

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /bottle/requirements-pypy.txt

CMD gunicorn app:app -c gunicorn_conf.py
