FROM python:2.7.15-stretch

ADD ./ /uw

WORKDIR /uw

RUN pip install -r /uw/requirements.txt

EXPOSE 8080

CMD uwsgi --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive --http-processes $(nproc) -p $(nproc) -w hello --add-header "Connection: keep-alive" --pidfile /tmp/uwsgi.pid
