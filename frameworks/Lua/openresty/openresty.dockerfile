FROM techempower/openresty-server:0.1

ADD ./nginx.conf /openresty/
ADD ./app.lua /openresty/

WORKDIR /openresty

RUN sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' app.lua

RUN luarocks install lua-resty-template

CMD nginx -c /openresty/nginx.conf -g "worker_processes '"${CPU_COUNT}"';"
