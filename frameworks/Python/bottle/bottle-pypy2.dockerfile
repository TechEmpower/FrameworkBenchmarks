FROM pypy:2-5.10

WORKDIR /bottle
COPY views views
COPY app.py app.py
COPY gunicorn_conf.py gunicorn_conf.py
COPY requirements-pypy.txt requirements-pypy.txt

RUN pip install -r requirements-pypy.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
