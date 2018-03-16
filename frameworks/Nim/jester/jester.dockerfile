FROM tfb/nimble:latest

# 2018-02-28
RUN git clone https://github.com/dom96/jester.git && \
    cd jester && \
    git checkout 86744f7dff56522c9baa1b4f4381db44044abd9f && \
    nimble update && \
    echo 'y' | nimble install

ENV JESTER_HOME=/jester

COPY ./ ./

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh
