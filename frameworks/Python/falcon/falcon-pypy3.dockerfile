FROM pypy:3.8-7.3

RUN apt-get update; apt-get install libpq-dev python3-dev -y
COPY ./ /falcon
WORKDIR /falcon
RUN pip3 install -U pip; pip3 install -r /falcon/requirements-pypy.txt

EXPOSE 8080

CMD ["gunicorn", "app:wsgi", "-c", "gunicorn_conf.py"]
