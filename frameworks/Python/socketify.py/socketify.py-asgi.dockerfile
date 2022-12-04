FROM pypy:3.9-bullseye

WORKDIR /usr/src/app

COPY requirements.txt ./

RUN apt-get update
RUN apt install libuv1-dev -y
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 3000

CMD WORKER_COUNT=$(nproc) pypy3 ./raw-asgi.py