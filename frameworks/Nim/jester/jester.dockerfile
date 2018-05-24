FROM nimlang/nim:0.16.0

RUN apt update -yqq && apt install -yqq nginx

# 2016-10-01
RUN git clone https://github.com/dom96/jester.git && \
    cd jester && \
    git checkout 22f6ce61924a8f4d170c54f1a6709f898085deb4 && \
    nimble update && \
    echo 'y' | nimble install

ENV JESTER_HOME=/jester

COPY ./ ./

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh
