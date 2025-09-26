FROM python:3.8-buster

RUN apt-get update; apt-get install libpq-dev python3-dev libev-dev -y
COPY ./ /falcon
WORKDIR /falcon
RUN pip3 install -U pip; pip3 install -r /falcon/requirements-bjoern.txt

EXPOSE 8080

CMD ["python3", "app_bjoern.py"]
