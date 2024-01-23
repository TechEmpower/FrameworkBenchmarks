FROM golang:1.14

RUN apt-get update -yqq

ADD ./ /aah
WORKDIR /aah

ENV GOPATH /aah
ENV PATH ${GOPATH}/bin:${PATH}
ENV GO111MODULE on

RUN curl -sL https://aahframework.org/install-cli | bash -s v0.13.3

WORKDIR /aah/src/benchmark

RUN aah --version
RUN mkdir -p views/common
RUN aah build --single

EXPOSE 8080

CMD build/bin/benchmark run --envprofile bm_default
