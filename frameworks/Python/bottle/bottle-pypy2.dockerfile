FROM techempower/pypy2:0.1

ADD ./ /bottle

WORKDIR /bottle

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /bottle/requirements-pypy.txt

CMD gunicorn app:app -c gunicorn_conf.py
