FROM pypy:3.9-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
COPY ./ /falcon
WORKDIR /falcon
RUN pip3 install -U pip
RUN pip3 install -r /falcon/requirements.txt
RUN pip3 install gunicorn==20.1.0
RUN pip3 install -r /falcon/requirements-db-pony.txt

EXPOSE 8080

CMD ["gunicorn", "app:wsgi", "-c", "gunicorn_conf.py"]
