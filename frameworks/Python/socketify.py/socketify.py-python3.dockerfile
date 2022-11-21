FROM python:3.11-rc

WORKDIR /usr/src/app

COPY requirements-python3.txt ./
RUN apt-get update
RUN apt install libuv1-dev -y
RUN pip install --no-cache-dir -r requirements-python3.txt

COPY . .

EXPOSE 3000

CMD WORKER_COUNT=$(nproc) python ./app-python3.py