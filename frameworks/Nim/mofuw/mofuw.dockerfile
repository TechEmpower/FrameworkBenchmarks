FROM gcc:7

RUN apt update -yqq &&                                                  \
    mkdir -p /nim &&                                                    \
    cd /nim &&                                                          \
    git clone -b devel https://github.com/nim-lang/Nim.git nim-devel && \
    cd nim-devel &&                                                     \
    git clone --depth 1 https://github.com/nim-lang/csources.git &&     \
    cd csources &&                                                      \
    sh build.sh &&                                                      \
    cd ../ &&                                                           \
    bin/nim c koch &&                                                   \
    ./koch boot -d:release &&                                           \
    ./koch nimble &&                                                    \
    ./koch tools

ENV PATH=$PATH:/nim/nim-devel/bin:/root/.nimble/bin

RUN git clone https://github.com/2vg/mofuw.git && \
    cd mofuw && \
    nimble update && \
    echo 'y' | nimble install

WORKDIR /app

COPY ./ /app

RUN chmod a+wrx start-servers.sh

CMD ./start-servers.sh