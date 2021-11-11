FROM pypy:2-5.10

ADD ./ /weppy

WORKDIR /weppy

RUN pip install -r /weppy/requirements-pypy.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
