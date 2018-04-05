FROM techempower/nginx:0.1

FROM techempower/python3:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /flask

WORKDIR /flask

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /flask/requirements.txt

RUN sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' /flask/nginx.conf

CMD nginx -c /flask/nginx.conf && uwsgi --ini /flask/uwsgi.ini --processes $(($(nproc)*3)) --wsgi app:app
