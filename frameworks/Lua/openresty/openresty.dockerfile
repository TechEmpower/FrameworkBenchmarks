FROM openresty/openresty:1.19.9.1-focal

ADD ./nginx.conf /openresty/
ADD ./app.lua /openresty/

WORKDIR /openresty

RUN luarocks install lua-resty-template

EXPOSE 8080

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    nginx -c /openresty/nginx.conf
