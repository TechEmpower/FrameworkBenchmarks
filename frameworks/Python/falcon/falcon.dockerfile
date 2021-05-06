FROM python:2.7.15-stretch

WORKDIR /falcon
COPY app.py app.py
COPY gunicorn_conf.py gunicorn_conf.py
COPY requirements.txt requirements.txt

RUN pip install -r requirements.txt

EXPOSE 8080

CMD ["gunicorn", "app:app", "-c", "gunicorn_conf.py"]
