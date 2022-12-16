FROM python:3.10-bullseye

RUN apt-get update; apt-get install libpq-dev python3-dev -y
COPY ./ /falcon
WORKDIR /falcon
RUN pip3 install -U pip; \
    pip3 install cython==0.29.26; \
    pip3 install -r /falcon/requirements.txt; \
    pip3 install falcon==3.1.1 --no-binary :all:;

EXPOSE 8080

CMD ["uvicorn", "asgi_tortoise:asgi", "--host", "0.0.0.0", "--port", "8080", "--workers", "2"]
