FROM pypy:2-5.10

ADD ./ /weppy

WORKDIR /weppy

RUN pip install -r /weppy/requirements-pypy.txt

CMD gunicorn app:app -c gunicorn_conf.py
