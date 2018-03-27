FROM tfb/php7:latest

ADD ./ /fat-free
WORKDIR /fat-free

ENV F3DIR="/fat-free/src"

RUN git clone "https://github.com/bcosca/fatfree-core.git" src
RUN cd src && git checkout -q "069ccd84afd2461c7ebb67f660c142f97577e661" # v3.5.2-dev

RUN chmod -R 777 /fat-free
