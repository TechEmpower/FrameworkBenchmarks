FROM python:3.9-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
WORKDIR /falcon
COPY ./ /falcon
RUN pip3 install -U pip; \
    pip3 install cython==0.29.26; \
    pip3 install -r /falcon/requirements.txt; \
    pip3 install falcon==3.1.1 --no-binary :all:;

ENV ASYNCIO=true

EXPOSE 8080

CMD ["gunicorn", "asgi_tortoise:asgi", "-c", "gunicorn_conf.py"]
