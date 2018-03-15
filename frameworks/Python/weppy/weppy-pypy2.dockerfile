FROM tfb/pypy2:latest

ADD ./ /weppy

WORKDIR /weppy

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /weppy/requirements-pypy.txt

CMD gunicorn app:app -c gunicorn_conf.py
