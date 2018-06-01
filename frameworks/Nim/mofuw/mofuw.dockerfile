FROM gcc:latest

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
    ./koch tools

ENV PATH $PATH:/nim/nim-devel/bin:/root/.nimble/bin

RUN nimble install -y mofuw

WORKDIR /mofuwApp
COPY techempower.nim techempower.nim
RUN nim c -d:release --threads:on -d:bufSize:512 techempower.nim
CMD ["./techempower"]
