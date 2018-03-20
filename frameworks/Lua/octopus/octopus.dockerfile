FROM tfb/lua:latest

WORKDIR /octo

RUN git clone https://github.com/cyberz-eu/octopus.git

WORKDIR octopus
# January 4th, 2017
RUN git checkout 0c4fc42198fed3a299c78d4b910188113d478bc5

WORKDIR /octo

# The following line is a hacky way to get this framework working.
# zlib fix needs to happen within the framework owner's repo
RUN sed -i 's|zlib_url=http://zlib.net/zlib-$zlib_version.tar.gz|zlib_url=https://github.com/madler/zlib/archive/v$zlib_version.tar.gz|g' octopus/bin/unix/server.sh

RUN ls -la

ADD ./app/ /octo/


RUN cp -avr app octopus/extensions
RUN cp -vf config.lua octopus/extensions

RUN sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' octopus/extensions/config.lua

WORKDIR octopus/bin/unix

RUN . ./server.sh install
RUN . ./server.sh build

CMD . ./server.sh start
