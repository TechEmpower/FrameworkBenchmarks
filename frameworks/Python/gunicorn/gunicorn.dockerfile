FROM python:3.9

ADD ./ /gunicorn

WORKDIR /gunicorn

RUN pip install -r /gunicorn/requirements.txt

EXPOSE 8080

CMD ["gunicorn", "app:main", "-c", "gunicorn_conf.py"]
