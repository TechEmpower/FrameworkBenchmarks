FROM python:3.10-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
WORKDIR /falcon
COPY ./ /falcon
RUN pip3 install -U pip
RUN pip3 install cython==0.29.26
#RUN pip3 install falcon==3.1.1 --no-binary :all:
RUN pip3 install -r /falcon/requirements.txt
RUN pip3 install -r /falcon/requirements-uvicorn.txt
RUN pip3 install -r /falcon/requirements-db-tortoise.txt

ENV ASYNCIO=true

EXPOSE 8080

CMD ["gunicorn", "app_asgi_tortoise:asgi", "-c", "gunicorn_conf.py"]
