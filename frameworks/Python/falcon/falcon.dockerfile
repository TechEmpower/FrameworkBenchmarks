FROM python:3.8-buster

RUN apt-get update; apt-get install libpq-dev python3-dev -y
COPY ./ /falcon
WORKDIR /falcon
RUN pip3 install -U pip; \
    pip3 install cython==0.29.26; \
    pip3 install falcon==3.0.1 --no-binary :all:; \
    pip3 install -r /falcon/requirements.txt

EXPOSE 8080

CMD ["gunicorn", "app:wsgi", "-c", "gunicorn_conf.py"]
