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

ENV PATH $PATH:/nim/nim-devel/bin:/root/.nimble/bin

WORKDIR /mofu_framework
RUN git clone https://github.com/2vg/mofuw.git && \
    cd mofuw && \
    nimble update && \
    echo 'y' | nimble install

WORKDIR /mofuw_app
COPY techempower.nim techempower.nim
RUN nim c -d:release --threads:on techempower.nim
RUN find /
CMD ["./techempower"]
