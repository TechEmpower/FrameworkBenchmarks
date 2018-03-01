FROM tfb/openresty-server:latest

COPY ./ ./

RUN luarocks install lua-resty-template
RUN luarocks install lapis

CMD echo -e "export DBHOST=`getent hosts TFB-database | awk '{ print $1 }'`"; lapis server production
