FROM techempower/nginx:0.1

FROM techempower/python3:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /bottle

WORKDIR /bottle

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /bottle/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' /bottle/nginx.conf

CMD nginx -c /bottle/nginx.conf && uwsgi --ini /bottle/uwsgi.ini --processes $((CPU_COUNT*3)) --wsgi app:app
