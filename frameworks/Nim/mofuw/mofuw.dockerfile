FROM tfb/nimble:latest

RUN git clone --depth 1 https://github.com/2vg/mofuw.git && \
    cd mofuw && \
    echo 'y' | nimble install

COPY ./ ./

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh
