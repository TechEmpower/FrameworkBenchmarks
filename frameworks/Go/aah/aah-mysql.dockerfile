FROM golang:1.10.1

RUN apt update -yqq && apt install unzip

ADD ./ /aah
WORKDIR /aah

RUN mkdir bin
ENV GOPATH /aah
ENV PATH ${GOPATH}/bin:${PATH}

RUN curl -sL -o install_glide.sh https://glide.sh/get
RUN sh install_glide.sh

WORKDIR src/benchmark
RUN glide -v
RUN glide install

RUN curl -sL -o /tmp/aah-linux-amd64.zip  https://cdn.aahframework.org/releases/cli/0.12.1/aah-linux-amd64.zip
RUN unzip -q /tmp/aah-linux-amd64.zip -d ${GOPATH}/bin/

RUN aah -v
RUN aah -y migrate code
RUN mkdir -p views/common
RUN aah build -s

CMD build/bin/benchmark -profile bm_mysql