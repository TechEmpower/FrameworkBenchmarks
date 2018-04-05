FROM techempower/nimble:0.1

# 2015-06-25
RUN git clone https://github.com/dom96/jester.git && \
    cd jester && \
    git checkout 71b8cc069a0d271d619c2dc41bc6479047885587 && \
    nimble update && \
    echo 'y' | nimble install

ENV JESTER_HOME=/jester

COPY ./ ./

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh
