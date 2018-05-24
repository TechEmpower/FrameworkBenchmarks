FROM golang:1.10.1

ADD ./ /beego
WORKDIR /beego

RUN mkdir bin
ENV GOPATH /beego
ENV PATH ${GOPATH}/bin:${PATH}

RUN curl -sL -o install_glide.sh https://glide.sh/get
RUN sh install_glide.sh

RUN glide -v
WORKDIR src/hello
RUN glide install
CMD go run main.go
