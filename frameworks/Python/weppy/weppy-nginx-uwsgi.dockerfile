FROM techempower/nginx:0.1

FROM techempower/python2:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /weppy

WORKDIR /weppy

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /weppy/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' /weppy/nginx.conf

CMD nginx -c /weppy/nginx.conf && uwsgi --ini /weppy/uwsgi.ini --processes $CPU_COUNT --wsgi app:app
