FROM python:3.9.7

ADD ./ /japronto

WORKDIR /japronto

RUN pip3 install -r /japronto/requirements_postgres.txt

EXPOSE 8080

CMD python3 app_postgres.py
