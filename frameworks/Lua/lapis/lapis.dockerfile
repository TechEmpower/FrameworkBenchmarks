FROM techempower/openresty-server:0.1

COPY ./ ./

RUN luarocks install lua-resty-template
RUN luarocks install lapis

# RUN DBHOST=`getent hosts tfb-database | awk '{ print $1 }'` sed -i 's|DBHOSTNAME|$DBHOST|g' nginx.conf

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|DBHOSTNAME|$DBIP|g" nginx.conf && \
    sed -i "s|DBHOSTNAME|$DBIP|g" config.moon && \
    sed -i "s|DBHOSTNAME|$DBIP|g" config.lua && \
    lapis server production
