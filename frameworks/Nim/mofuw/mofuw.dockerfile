FROM nimlang/nim:0.18.0

RUN apt update -yqq

RUN git clone https://github.com/2vg/mofuw.git && \
    cd mofuw && \
    nimble update && \
    echo 'y' | nimble install

COPY ./ ./

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh
