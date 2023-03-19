FROM python:3.9-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
WORKDIR /falcon
COPY ./ /falcon
RUN pip3 install -U pip; pip3 install -r /falcon/requirements-fastwsgi.txt

EXPOSE 8080

CMD ["python3", "app_fastwsgi.py"]
