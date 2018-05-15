FROM golang:1.10.1

ADD ./ /echo
WORKDIR /echo

RUN mkdir bin
ENV GOPATH /echo
ENV PATH ${GOPATH}/bin:${PATH}

RUN curl -sL -o install_glide.sh https://glide.sh/get
RUN sh install_glide.sh

WORKDIR src
RUN glide install
WORKDIR ..
RUN go install app

CMD app
