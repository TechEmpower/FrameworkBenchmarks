FROM python:3.11-rc-alpine

WORKDIR /usr/src/app

COPY requirements-python3.txt ./

RUN apk add --no-cache libuv-dev git openssl-dev libffi-dev build-base python3-dev

RUN pip install --no-cache-dir -r requirements-python3.txt

COPY . .

EXPOSE 3000

CMD WORKER_COUNT=$(nproc) python ./app-python3.py