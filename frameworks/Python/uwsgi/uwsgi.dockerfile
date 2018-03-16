FROM tfb/python2:latest

ADD ./ /uw

WORKDIR /uw

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /uw/requirements.lock

CMD uwsgi --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive --http-processes $CPU_COUNT -p $CPU_COUNT -w hello --add-header "Connection: keep-alive" --pidfile /tmp/uwsgi.pid
