FROM pypy:2-5.10

WORKDIR /falcon
COPY app.py app.py
COPY gunicorn_conf.py gunicorn_conf.py
COPY requirements-pypy.txt requirements-pypy.txt

RUN pip install -r requirements-pypy.txt

CMD ["gunicorn", "app:app", "-c", "gunicorn_conf.py"]
