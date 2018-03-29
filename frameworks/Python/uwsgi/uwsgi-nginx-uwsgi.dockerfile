FROM tfb/nginx:latest

FROM tfb/python2:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /uw

WORKDIR /uw

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /uw/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' /uw/nginx.conf

CMD nginx -c /uw/nginx.conf && uwsgi --ini uwsgi.ini --processes $(nproc) --gevent 1000 --wsgi hello
