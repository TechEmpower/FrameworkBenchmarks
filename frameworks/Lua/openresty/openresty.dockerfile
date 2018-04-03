FROM techempower/openresty-server:0.1

ADD ./nginx.conf /openresty/
ADD ./app.lua /openresty/

WORKDIR /openresty

RUN luarocks install lua-resty-template

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|DBHOSTNAME|$DBIP|g" app.lua && \
    nginx -c /openresty/nginx.conf -g "worker_processes '"$(nproc)"';"
