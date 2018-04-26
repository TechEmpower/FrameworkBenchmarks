FROM python:3.6.5

WORKDIR /falcon
COPY app.py app.py
COPY gunicorn_conf.py gunicorn_conf.py
COPY requirements.txt requirements.txt

RUN pip3 install -r requirements.txt

CMD ["gunicorn", "app:app", "-c", "gunicorn_conf.py"]
