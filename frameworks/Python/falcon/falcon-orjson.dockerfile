FROM python:3.9-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
WORKDIR /falcon
COPY ./ /falcon
RUN pip3 install -U pip
RUN pip3 install cython==0.29.26
RUN pip3 install orjson==3.8.10
#RUN pip3 install falcon==3.1.1 --no-binary :all:
RUN pip3 install -r /falcon/requirements.txt
RUN pip3 install -r /falcon/requirements-meinheld.txt
RUN pip3 install -r /falcon/requirements-db-pony.txt

EXPOSE 8080

ENV USE_ORJSON=1

CMD ["gunicorn", "app:wsgi", "-c", "gunicorn_conf.py"]
