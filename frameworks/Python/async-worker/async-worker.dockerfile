FROM python:3.9-alpine

LABEL description="Image used to run async-worker benchmark tests."
LABEL version='0.1'

ENV ASYNCWORKER_HTTP_HOST=0.0.0.0
ENV ASYNCWORKER_HTTP_PORT=8080

WORKDIR /app

COPY /src /app
COPY Pipfile /app
COPY Pipfile.lock /app

RUN apk add --virtual .deps gcc g++ make openssl-dev libxml2 libffi-dev && \
    pip install pipenv && \
    pipenv install --system --ignore-pipfile

CMD ["python", "./hello_world.py"]
