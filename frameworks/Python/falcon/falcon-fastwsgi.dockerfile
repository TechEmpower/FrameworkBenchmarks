FROM python:3.11-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
WORKDIR /falcon
COPY ./ /falcon
RUN pip3 install -U pip
RUN pip3 install -r /falcon/requirements.txt
RUN pip3 install -r /falcon/requirements-fastwsgi.txt
RUN pip3 install -r /falcon/requirements-db-pony.txt

EXPOSE 8080

CMD ["python3", "app.py", "-s", "fastwsgi"]
