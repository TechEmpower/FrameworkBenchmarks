FROM techempower/lua:0.1

WORKDIR /octo

RUN git clone https://github.com/cyberz-eu/octopus.git

WORKDIR /octo/octopus
# Dec 8th, 2017
RUN git checkout 44c7e7ecdfd9e95703e73df85815c0cca4b441e8

WORKDIR /octo

ADD ./ /octo

RUN cp -avr app octopus/extensions
RUN cp -vf config.lua octopus/extensions

WORKDIR /octo/octopus/bin/unix

RUN chmod +x *.sh

RUN sed -i 's|wget|wget -q|g' server.sh
RUN sed -i 's|-c nginx.conf|-c nginx.conf -g "daemon off;"|g' server.sh

RUN ./server.sh install
RUN ./server.sh build

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|DBHOSTNAME|$DBIP|g" /octo/octopus/extensions/build/src/types.lua && \
    ./server.sh start
