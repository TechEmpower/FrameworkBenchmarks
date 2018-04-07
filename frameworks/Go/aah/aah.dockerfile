FROM golang:1.10.1

RUN apt update -yqq && apt install unzip

ADD ./ /aah
WORKDIR /aah

RUN mkdir bin
ENV GOPATH /aah
ENV PATH ${GOPATH}/bin:${PATH}

RUN mkdir src/aahframework.org
RUN git clone https://github.com/go-aah/tools.git src/aahframework.org/tools.v0
WORKDIR src/aahframework.org/tools.v0
RUN git checkout tags/v0.8 -b v0.8

WORKDIR /aah
RUN go get aahframework.org/tools.v0/aah/...
RUN go install aahframework.org/tools.v0/aah
RUN aah -v

RUN curl -sL -o install_glide.sh https://glide.sh/get
RUN sh install_glide.sh

RUN glide -v

RUN rm -rf src/aahframework.org src/golang.org src/gopkg.in

WORKDIR src/benchmark
RUN glide install
RUN aah build -o build/benchmark.zip
WORKDIR build
RUN unzip benchmark.zip

WORKDIR /aah/src/benchmark
CMD build/benchmark/bin/benchmark --profile=prod
