FROM python:3.9-bullseye

RUN apt-get update -yqq
RUN apt-get install python3-dev -y

WORKDIR /uw
COPY ./ /uw
RUN pip3 install -U pip; pip3 install -r /uw/requirements.txt

EXPOSE 8080

CMD uwsgi --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive --http-processes $(nproc) -p $(nproc) -w hello --add-header "Connection: keep-alive" --pidfile /tmp/uwsgi.pid
